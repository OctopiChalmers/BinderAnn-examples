{-# OPTIONS_GHC -fplugin BinderAnn.Monadic #-}
module Main (main) where

import Control.Shell
import BinderAnn.Monadic

main :: IO ()
main = do
  res <- shell' saveInfo
  print res

cpuinfo = capture (run "cat" ["/proc/cpuinfo"])
meminfo = capture (run "cat" ["/proc/meminf"])

saveInfo :: Shell ()
saveInfo = do
  cpu <- cpuinfo
  mem <- meminfo
  output "info.txt" (cpu ++ mem)


