{-# LANGUAGE CPP, GADTs, RecordWildCards, FlexibleInstances, MultiParamTypeClasses #-}
-- | Primitives and run function.
module Control.Shell.Internal
  ( Shell
  , ExitReason (..), Env (..)
  , shell, shell', runSh
  , exit, run, try, getEnv, inEnv, unsafeLiftIO, (|>)
  ) where
import Control.Monad (when, ap)
import qualified Control.Concurrent as Conc
import qualified Control.Exception as Ex
import qualified System.Exit as Exit
import qualified System.Process as Proc
import qualified System.IO as IO
import qualified System.Directory as Dir (getCurrentDirectory)
import qualified System.Environment as Env (getEnvironment)
import qualified System.Info as Info (os)

import BinderAnn.Monadic

-- | A command name plus a ProcessHandle.
data Pid = PID !String                         !Proc.ProcessHandle
         | TID !(Conc.MVar (Maybe ExitReason)) !Conc.ThreadId

-- | A shell environment: consists of the current standard input, output and
--   error handles used by the computation, as well as the current working
--   directory and set of environment variables.
data Env = Env
  { envStdIn   :: !IO.Handle
  , envStdOut  :: !IO.Handle
  , envStdErr  :: !IO.Handle
  , envWorkDir :: !FilePath
  , envEnvVars :: ![(String, String)]
  }

-- | A shell command: either an IO computation or a pipeline of at least one
--   step.
data Shell a where
  Lift   :: !(IO a) -> Shell a
  Pipe   :: ![PipeStep] -> Shell ()
  Bind   :: Shell a -> (a -> Shell b) -> Shell b
  GetEnv :: Shell Env
  InEnv  :: Env -> Shell a -> Shell a
  Try    :: Shell a -> Shell (Either String a)
  Done   :: Shell a
  Fail   :: String -> Shell a
  Tag    :: [SrcInfo] -> Shell a -> Shell a

-- | A step in a pipeline: either a shell computation or an external process.
data PipeStep
  = Proc     !String ![String]
  | Internal !(Shell ())

instance Functor Shell where
  fmap f m = m >>= return . f

instance Applicative Shell where
  (<*>) = ap
  pure  = return

instance Monad Shell where
  return = Lift . return
  (>>=)  = Bind
  fail   = Fail

-- | Lift an IO computation into a shell. The lifted computation is not
--   thread-safe, and should thus absolutely not use environment variables,
--   relative paths or standard input/output.
unsafeLiftIO :: IO a -> Shell a
unsafeLiftIO = Lift

-- | Why did the computation terminate?
data ExitReason = Success | Failure !String [SrcInfo]
  deriving (Show, Eq)

-- | Run a shell computation. If part of the computation fails, the whole
--   computation fails. The computation's environment is initially that of the
--   whole process.
shell :: Shell a -> IO (Either ExitReason a)
shell m = do
    evs <- Env.getEnvironment
    wd <- Dir.getCurrentDirectory
    runSh [] (env wd evs) m
  where
    env wd evs = Env
      { envStdIn   = IO.stdin
      , envStdOut  = IO.stdout
      , envStdErr  = IO.stderr
      , envWorkDir = wd
      , envEnvVars = evs
      }


shell' :: Shell a -> IO a
shell' m = do
  res <- shell m 
  case res of
    Right a -> return a
    Left (Failure msg []) -> error msg
    Left (Failure msg (Info nm loc : _)) -> error $ concat
      [
      case loc of
        Nothing -> ""
        Just (file, r, c) -> 
          "Exception raised at " ++ file ++"(" ++ show r ++ "," ++ show c ++ "):\n", 
      case nm of 
        Nothing -> ""
        Just name -> "The value " ++ "\"" ++ name ++ "\" produced the following error:\n",
      msg
      ]

instance Annotated SrcInfo Shell a where
  annotateM ma info = Tag [info] ma


runSh :: [SrcInfo] -> Env -> Shell a -> IO (Either ExitReason a)
runSh info _ (Lift m) = do
  Ex.catch (Right <$> m)
           (\(Ex.SomeException e) -> pure $ Left (Failure (show e) info))
runSh info env (Pipe p) = flip Ex.catch except $ do
  steps <- mkEnvs env p
  pids <- mapM (uncurry (runStep info closeFDs)) steps
  ma <- waitPids info pids
  case ma of
    Failure err info' -> pure $ Left (Failure err info')
    _                 -> pure $ Right ()
  where
    closeFDs
      | Info.os == "mingw32" = False
      | otherwise            = True
    except = \(Ex.SomeException e) -> pure $ Left (Failure (show e) info)
runSh _info _ Done = do
  return $ Left Success
runSh info env (Bind m f) = do
  res <- runSh info env m
  case res of
    Right x -> runSh info env (f x)
    Left e  -> pure $ Left e
runSh _info env GetEnv = do
  pure $ Right env
runSh info _ (InEnv env m) = do
  runSh info env m
runSh info env (Try m) = do
  res <- runSh info env m
  case res of
    Right x          -> pure $ Right (Right x)
    Left (Failure e _info') -> pure $ Right (Left e)
    Left Success     -> pure $ Left Success
runSh info _ (Fail e) = do
  pure $ Left (Failure e info)
runSh info env (Tag info' sh) = do
  runSh info' env sh


-- | Start a pipeline step.
runStep :: [SrcInfo] -> Bool -> Env -> PipeStep -> IO Pid
runStep _info closefds Env{..} (Proc cmd args) = do
    (_, _, _, ph) <- Proc.createProcess cproc
    pure $ PID cmd ph
  where
    cproc = Proc.CreateProcess
      { Proc.cmdspec      = Proc.RawCommand cmd args
      , Proc.cwd          = Just envWorkDir
      , Proc.env          = Just envEnvVars
      , Proc.std_in       = Proc.UseHandle envStdIn
      , Proc.std_out      = Proc.UseHandle envStdOut
      , Proc.std_err      = Proc.UseHandle envStdErr
      , Proc.close_fds    = closefds
      , Proc.create_group = False
#if MIN_VERSION_process(1,2,0)
      , Proc.delegate_ctlc = False
#endif
#if MIN_VERSION_process(1,3,0)
      , Proc.detach_console     = False
      , Proc.create_new_console = False
      , Proc.new_session        = False
      , Proc.child_group        = Nothing
      , Proc.child_user         = Nothing
#endif
#if MIN_VERSION_process(1,5,0)
      , Proc.use_process_jobs   = False
#endif
      }
runStep info closefds env (Internal cmd) = do
  v <- Conc.newEmptyMVar
  tid <- Conc.forkFinally (runSh info env cmd >>= done) $ \res -> do
    case res of
      Right (Left e) -> Conc.putMVar v (Just e)
      Left e         -> Conc.putMVar v (Just $ Failure (show e) info)
      _              -> Conc.putMVar v Nothing
  pure $ TID v tid
  where
    done x = do
      when closefds $ IO.hClose (envStdOut env)
      return x

-- | Pair up pipe steps with corresponding environments, ensuring that each
--   step is connected to the next via a pipe.
mkEnvs :: Env -> [PipeStep] -> IO [(Env, PipeStep)]
mkEnvs env = go [] (envStdIn env)
  where
    go acc stdi [step] = do
      let env' = env {envStdIn = stdi, envStdOut = envStdOut env}
      pure ((env', step) : acc)
    go acc stdi (step : steps) = do
      (next, stdo) <- Proc.createPipe
      go ((env {envStdIn = stdi, envStdOut = stdo}, step):acc) next steps
    go acc _ _ = pure acc

-- | Terminate a pid, be it process or thread.
killPid :: Pid -> IO ()
killPid (PID _ p) = Proc.terminateProcess p
killPid (TID _ t) = Conc.killThread t

-- | Wait for all processes in the given list. If a process has failed, its
--   error message is returned and the rest are killed.
waitPids :: [SrcInfo] -> [Pid] -> IO ExitReason
waitPids info (PID cmd p : ps) = do
  exCode <- Proc.waitForProcess p
  case exCode of
    Exit.ExitFailure ec -> do
      mapM_ killPid ps
      return
        (Failure (concat ["Command `", cmd, "' failed with error code ", show ec])
          info)
    _ -> do
      waitPids info ps
waitPids info (TID v _ : ps) = do
  merr <- Conc.takeMVar v
  case merr of
    Just e -> mapM_ killPid ps >> return e
    _      -> waitPids info ps
waitPids _info _ = do
  return Success

-- | Execute an external command. No globbing, escaping or other external shell
--   magic is performed on either the command or arguments. The program's
--   stdout will be written to stdout.
run :: FilePath -> [String] -> Shell ()
run p args = Pipe [Proc p args]

-- | Terminate the program successfully.
exit :: Shell a
exit = Done

-- | Connect the standard output of the first argument to the standard input
--   of the second argument, and run the two computations in parallel.
(|>) :: Shell () -> Shell () -> Shell ()
Pipe m |> Pipe n = Pipe (m ++ n)
Pipe m |> n      = Pipe (m ++ [Internal n])
m      |> Pipe n = Pipe (Internal m : n)
m      |> n      = Pipe [Internal m, Internal n]
infixl 5 |>

-- | Run a computation in the given environment.
inEnv :: Env -> Shell a -> Shell a
inEnv = InEnv

-- | Get the current environment.
getEnv :: Shell Env
getEnv = GetEnv

-- | Attempt to run a computation. If the inner computation fails, the outer
--   computations returns its error message, otherwise its result is returned.
try :: Shell a -> Shell (Either String a)
try = Try
