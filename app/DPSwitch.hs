{-# LANGUAGE Arrows #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module DPSwitch where

import qualified Control.Arrow as Bifunctor
import Data.Foldable (sequenceA_, traverse_)
import Data.Interval
import Data.Maybe (fromMaybe)
import Data.MonadicStreamFunction.InternalCore
import Data.Traversable as T
import FRP.BearRiver (
  Event (Event, NoEvent),
  SF,
  boolToEvent,
  dSwitch,
  dpSwitchB,
  dup,
  edge,
  iEdge,
  integral,
  lMerge,
  loopPre,
  mergeBy,
  notYet,
  parB,
  parC,
  rMerge,
  switch,
  tag,
  time,
 )

-- dpSwitch is a part of Yampa but it has not been ported in any official Dunai release.
-- But there exists a pull request implementing dpSwitch here:
-- https://github.com/ivanperez-keera/dunai/pull/316
-- I took that implementation and adapted it

dpSwitch ::
  (Monad m, Traversable col) =>
  (forall sf. (a -> col sf -> col (b, sf))) ->
  col (FRP.BearRiver.SF m b c) ->
  SF m (a, col c) (FRP.BearRiver.Event d) ->
  (col (SF m b c) -> d -> SF m a (col c)) ->
  SF m a (col c)
dpSwitch rf sfs sfF sfCs = MSF $ \a -> do
  let bsfs = rf a sfs
  res <- T.mapM (\(b, sf) -> unMSF sf b) bsfs
  let cs = fmap fst res
      sfs' = fmap snd res
  (e, sfF') <- unMSF sfF (a, cs)
  let ct = case e of
        FRP.BearRiver.Event d -> sfCs sfs' d
        NoEvent -> dpSwitch rf sfs' sfF' sfCs
  return (cs, ct)
