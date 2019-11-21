{-# OPTIONS_GHC -fplugin BinderAnn.Pure #-}
module Main (main) where

import GCM
import CP
import Compile
import BinderAnn.Pure


main :: IO ()
main = do
  res <- run True True simpleExample
  putStr res

-- A source of flow a
source :: (CPType a) => a -> GCM (Port a)
source a = do
  p <- createPort
  set p a
  return p

-- A sink of capacity a
sink :: (CPType a, Ord a, Num a) => a -> GCM (Port a)
sink a = do
  p <- createPort
  component $ do
    inflow <- value p
    assert $ inflow `inRange` (0, lit a)
  return p

rain :: (Num a, CPType a) => a -> GCM (Port a)
-- Rain is a source of Floats
rain = source

-- A pipe with a fixed capacity
pipe :: (Num a, Ord a, CPType a) => a -> GCM (Port a)
pipe k = do
  flow <- createPort
  component $ do
    f <- value flow
    assert  $ f `inRange` (0, lit k)
  return flow

-- Storage as a GCM component
storage :: (Num a, Ord a, CPType a) => a -> GCM (Port a, Port a)
storage k = do
  ip <- createPort
  op <- createPort
  linkBy (fun (\inflow -> max' 0 (inflow - lit k))) ip op
  return (ip, op)

-- Fill inputs in order
fill :: (Num a, Ord a, CPType a) => Port a -> [(Maybe (Port a), Port a)] -> GCM ()
fill p [] = return ()
fill p ((Just cap, x):xs) = do
  residual <- createPort
  component $ do
    res <- value residual
    pv  <- value p
    xv  <- value x
    c   <- value cap
    assert $ xv  === max' 0 (min' c pv)
    assert $ res === pv - xv
  fill residual xs
fill p ((Nothing, x):xs) = do
  link p x
  mapM_ (\p -> set (snd p) 0) xs

-- A pump has a maximum capacity and a flow
pump :: (Num a, Ord a, CPType a) => a -> GCM (Port a, Port a)
pump cmax = do
  fp <- createPort
  cp <- createPort
  component $ do
    f <- value fp
    c <- value cp
    assert $ c `inRange` (0, lit cmax)
    assert $ f `inRange` (0, c)
  return (fp, cp)

minimize gp op = component $ do
  v <- value op
  g <- value gp
  assert (g === 0 - v)


-- Simple example with outputs and everything
simpleExample :: GCM ()
simpleExample = do

  -- The rain
  rain <- source 10

  -- Storage
  (inflow, overflow) <- storage 3

  -- The pump
  (flow, capacity) <- pump 6

  -- Rain first goes in to the pump, and then the store
  fill rain [(Just capacity, flow), (Nothing, inflow)]

  -- Minimise overflow goal
  goal <- createIntGoal
  goal `minimize` overflow

  -- Outputs
  output' capacity 
  output' flow 
  output' overflow 
  output' inflow 
 
  -- with redundant strings
  -- output capacity "capacity"   
  -- output flow "flow"
  -- output overflow "overflow"
  -- output inflow "inflow"
