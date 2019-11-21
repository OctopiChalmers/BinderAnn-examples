{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables, MultiParamTypeClasses #-}
module Program (Program(..), interpret, Interprets(..)) where
import Control.Monad
import Control.Monad.Fail as Fail

-- Generic programs, parameterised over some instruction set
data Program instr a where
    Return :: a -> Program instr a
    (:>>=) :: Program instr a -> (a -> Program instr b) -> Program instr b
    Instr  :: instr a -> Program instr a
    -- Annotate :: Maybe String -> Maybe (String, Int, Int)
    --          -> Program instr a -> Program instr a

-- Obvious instance
instance Monad (Program instr) where
    return = Return
    (>>=)  = (:>>=)

instance MonadFail (Program instr) where
  fail = Fail.fail

-- Obvious instance
instance Applicative (Program instr) where
    pure = return
    (<*>) = ap

-- Obvious instance
instance Functor (Program instr) where
    fmap = liftM

class Interprets m instr where
  interp :: instr b -> m b

-- Interpret
interpret :: (Monad m, Interprets m instr) => Program instr a -> m a
interpret (Return a)       = return a
interpret (m :>>= f)       = interpret m >>= (interpret . f)
interpret (Instr  i)       = interp i
-- interpret (Annotate n l m) = interpret m
