{-# OPTIONS_GHC -fplugin BinderAnn.Monadic #-}
module Main (main) where

import Text.Dot
import BinderAnn.Monadic

main :: IO ()
main = putStr (showDot semaphore)

semaphore :: Dot ()
semaphore = do
  green  <- node []
  yellow <- node []
  red    <- node []
  green  .->. yellow
  yellow .->. red
  red    .->. green
