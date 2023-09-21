{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module CollisionUtils where

import DPSwitch (dpSwitch)
import IdentityList
import Types

import Control.Monad.Trans.MSF
import Data.Functor.Identity
import Data.MonadicStreamFunction
import Data.MonadicStreamFunction.InternalCore (MSF (MSF, unMSF))
import FRP.BearRiver (parC)

import Data.IORef
import qualified Data.Text as T

import qualified Data.Bifunctor
import Data.Text (Text)

epsilon :: Double
epsilon = 0.01

-- MSF to put times until collision to continuous collision detection monad context
tellFutureTime :: MSF GameMonad FutureTime ()
tellFutureTime = liftTransS $ arrM tell

getImpactTimeWithAcc :: Double -> Double -> Double -> FutureTime
getImpactTimeWithAcc p v a
  | a == 0 = getImpactTime p v
  | abs p <= (epsilon / 2) = Infinity
  | otherwise = impactTimeWithAcc
 where
  discriminant
    | v < 0 = v * v - 2 * a * (p - (epsilon / 4))
    | otherwise = v * v - 2 * a * (p + (epsilon / 4))
  root1 = (-(sqrt discriminant) - v) / a
  root2 = (sqrt discriminant - v) / a
  impactTimeWithAcc
    | discriminant < 0 = Infinity
    | root1 > 0 && root2 > 0 = min (AtTime root1) (AtTime root2)
    | root1 > 0 || root2 > 0 = max (AtTime root1) (AtTime root2)
    | otherwise = Infinity

-- Needed to play with epsilons as there were some issues with double arithmethic and rounding
getImpactTime :: Double -> Double -> FutureTime
getImpactTime p v
  | p > epsilon / 2 && v < 0 = AtTime (-(p - (epsilon / 3)) / v)
  | p < -epsilon / 2 && v > 0 = AtTime (-(p + (epsilon / 3)) / v)
  | p > 0 && v < 0 = Infinity -- AtTime (-p/v)
  | p < 0 && v > 0 = Infinity -- AtTime (-p/v)
  | otherwise = Infinity

-- This function tells the time of an intersection between any of the infinitely extended parallel edges of two rectangles
-- (e.g. time when x coordinates of left sides of two rectangles intersect,
-- time when x coordinate of left edge of one box and right edge of another box intersect, etc.)
-- This can be optimised, so that only times of actual intersections of two rectangles are reported.
-- The only issue right now is that physics simulations might be told to be run more frequently than necessary,
-- which impacts performance
tellHittingTimeToBox :: MSF GameMonad (MovementState, MovementState) ()
tellHittingTimeToBox = proc (ms1@(MovementState box1 vel1 acc1), ms2@(MovementState pos2 vel2 acc2)) -> do
  tellFutureTime
    -<
      getImpactTimeWithAcc
        (pX pos2 + w pos2 - pX box1)
        (x vel2 - x vel1)
        (-x acc2 + x acc1)
  tellFutureTime
    -<
      getImpactTimeWithAcc
        (-pX pos2 + w box1 + pX box1)
        (-x vel2 + x vel1)
        (-x acc2 + x acc1)
  tellFutureTime
    -<
      getImpactTimeWithAcc
        (pY pos2 + h pos2 - pY box1)
        (y vel2 - y vel1)
        (-y acc2 + y acc1)
  tellFutureTime
    -<
      getImpactTimeWithAcc
        (-pY pos2 + h box1 + pY box1)
        (-y vel2 + y vel1)
        (-y acc2 + y acc1)

calcTimeToHit :: MSF GameMonad ([MovementState], MovementState) ()
calcTimeToHit = proc (trips, trip) -> do
  parC tellHittingTimeToBox -< map (\t -> (t, trip)) trips
  returnA -< ()

speedWhenGrounded :: Bool -> Bool -> Double -> Double
speedWhenGrounded blockedLow blockedHigh y
  | y < 0 && blockedLow = 0
  | y > 0 && blockedHigh = 0
  | otherwise = y

checkAABB :: Rct -> Rct -> Bool
checkAABB box1 box2 =
  (pX box1 + w box1) > pX box2
    && pX box1 < (pX box2 + w box2)
    && (pY box1 + h box1) > pY box2
    && pY box1 < (pY box2 + h box2)

getBlockingData :: Rct -> [Rct] -> BlockingData
getBlockingData (Rct x y w h) rcts =
  let u = foldr ((||) . checkAABB (Rct x (y + epsilon) w h)) False rcts
      r = foldr ((||) . checkAABB (Rct (x + epsilon) y w h)) False rcts
      d = foldr ((||) . checkAABB (Rct x (y - epsilon) w h)) False rcts
      l = foldr ((||) . checkAABB (Rct (x - epsilon) y w h)) False rcts
      ur = foldr ((||) . checkAABB (Rct (x + epsilon) (y + epsilon) w h)) False rcts
      dr = foldr ((||) . checkAABB (Rct (x + epsilon) (y - epsilon) w h)) False rcts
      dl = foldr ((||) . checkAABB (Rct (x - epsilon) (y - epsilon) w h)) False rcts
      ul = foldr ((||) . checkAABB (Rct (x - epsilon) (y + epsilon) w h)) False rcts
   in BlockingData
        u
        r
        d
        l
        ur
        dr
        dl
        ul
