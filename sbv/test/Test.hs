-----------------------------------------------------------------------------
-- |
-- Module    : Documentation.SBV.Examples.CodeGeneration.AddSub
-- Copyright : (c) Levent Erkok
-- License   : BSD3
-- Maintainer: erkokl@gmail.com
-- Stability : experimental
--
-- Simple code generation example.
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fplugin BinderAnn.Monadic #-}

module Main (main) where

import Data.SBV
import Data.SBV.Tools.CodeGen
import BinderAnn.Monadic


main :: IO ()
main = compileToC Nothing "addSub" genAddSub

genAddSub = do
  x <- cgInput' :: SBVCodeGen SInt32
  y <- cgInput'
  diff <- cgOutput' (x - y)
  cgReturn (x + y)
