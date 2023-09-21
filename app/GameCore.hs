{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module GameCore where

import CollisionUtils
import DPSwitch (dpSwitch)
import IdentityList
import Types

import Control.Monad.Trans.MSF
import Data.Functor.Identity
import Data.MonadicStreamFunction
import Data.MonadicStreamFunction.InternalCore (MSF (MSF, unMSF))
import FRP.BearRiver (
  Event (Event, NoEvent),
  SF,
  after,
  boolToEvent,
  dSwitch,
  dpSwitchB,
  dup,
  edge,
  iEdge,
  identity,
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

import Data.IORef
import qualified Data.Text as T

import qualified Data.Bifunctor
import qualified Data.Maybe as Data
import Data.Text (Text)

gravity :: Double
gravity = -100 * 9.81

jumpSpeed :: Double
jumpSpeed = 500

playerSideSpeed :: Double
playerSideSpeed = 150

enemySideSpeed :: Double
enemySideSpeed = 100

bounceSpeed :: Double
bounceSpeed = 1.20 * jumpSpeed

getObjectMovementState ::
  MovementState -> BlockingData -> ControlIntention -> MovementState
getObjectMovementState ms@(MovementState (Rct x0 y0 pWidth pHeight) (Vct vx0 vy0) (Vct accx0 accy0)) sBlockingData@(BlockingData u r d l ur dr dl ul) ci =
  let desired_vx
        | ciDir ci == DirLeft = -ciSideVel ci
        | ciDir ci == DirRight = ciSideVel ci
        | otherwise = 0

      desired_vy
        | ciBounce ci = ciBounceVel ci
        | (not u && d && ciJump ci) = ciJumpVel ci
        | otherwise = vy0

      starting_vx = speedWhenGrounded l r desired_vx
      starting_vy =
        speedWhenGrounded
          (d || (starting_vx < 0 && dl) || (starting_vx > 0 && dr))
          (u || (starting_vx < 0 && ul) || (starting_vx > 0 && ur))
          desired_vy

      desired_ax = 0.0
      desired_ay = ciGravity ci

      ax = speedWhenGrounded l r desired_ax
      ay =
        speedWhenGrounded
          (d || (starting_vx < 0 && dl) || (starting_vx > 0 && dr))
          (u || (starting_vx < 0 && ul) || (starting_vx > 0 && ur))
          desired_ay
   in MovementState
        (Rct x0 y0 pWidth pHeight)
        (Vct starting_vx starting_vy)
        (Vct ax ay)

extrapolateMovementState :: MovementState -> DTime -> MovementState
extrapolateMovementState (MovementState (Rct x0 y0 pWidth pHeight) (Vct vx0 vy0) (Vct ax0 ay0)) t =
  let
    current_vx = vx0 + (ax0 * t)
    current_vy = vy0 + (ay0 * t)
    current_x = x0 + (vx0 * t) + (ax0 * t * t / 2)
    current_y = y0 + (vy0 * t) + (ay0 * t * t / 2)
   in
    MovementState
      (Rct current_x current_y pWidth pHeight)
      (Vct current_vx current_vy)
      (Vct ax0 ay0)

playerObject ::
  (MovementState, BlockingData, ControlIntention, CtrlScheme) -> Object
playerObject (ms, sBlockingData@(BlockingData u r d l ur dr dl ul), oldCi, ctrlScheme) =
  proc oi -> do
    t <- FRP.BearRiver.time -< ()

    let triple = ms
    let currentTriple@(MovementState rct vel acc) = extrapolateMovementState triple t
    let dir
          | cLeft ctrlScheme (oiControlKeys oi) && cRight ctrlScheme (oiControlKeys oi) =
              DirNone
          | cLeft ctrlScheme (oiControlKeys oi) = DirLeft
          | cRight ctrlScheme (oiControlKeys oi) = DirRight
          | otherwise = DirNone

    let bounce = oiBounce oi == FRP.BearRiver.Event () && y vel < 100 && not u
    let jump =
          not u
            && d
            && cUp ctrlScheme (oiControlKeys oi)
            && not (cUp ctrlScheme $ ciControlKeys oldCi)

    returnA
      -<
        ( ObjOutput
            { ooObsObjState = OOSPlayer currentTriple sBlockingData ctrlScheme
            , ooRemoveReq = oiHit oi
            , ooControlIntention =
                ControlIntention
                  dir
                  jump
                  bounce
                  (oiControlKeys oi)
                  playerSideSpeed
                  jumpSpeed
                  bounceSpeed
                  gravity
            , ooControlIntentionChanged = oiControlKeys oi /= ciControlKeys oldCi
            }
        )

enemyObject ::
  (MovementState, BlockingData, ControlIntention, Maybe Double, Maybe Double) ->
  Object
enemyObject
  ( ms
    , sBlockingData@(BlockingData u r d l ur dr dl ul)
    , ci@(ControlIntention{ciDir = dir})
    , xMin
    , xMax
    ) =
    proc oi -> do
      t <- FRP.BearRiver.time -< ()

      let triple = ms
      let currentTriple@(MovementState rct vel acc) = extrapolateMovementState triple t
      let boundLeft = Data.fromMaybe (-100) xMin
      let boundRight = Data.fromMaybe 1000 xMax
      let canMoveLeft = not (pX rct < boundLeft || l)
      let canMoveRight = not (pX rct > boundRight || r)

      let newDir
            | not canMoveLeft && not canMoveRight = DirNone
            | not canMoveLeft && canMoveRight = DirRight
            | canMoveLeft && not canMoveRight = DirLeft
            | dir == DirNone = DirLeft
            | otherwise = dir
      changedControls <- FRP.BearRiver.iEdge False -< dir /= newDir

      returnA
        -<
          ( ObjOutput
              { ooObsObjState = OOSEnemy currentTriple sBlockingData xMin xMax
              , ooRemoveReq = oiHit oi
              , ooControlIntention =
                  ControlIntention
                    newDir
                    False
                    False
                    (oiControlKeys oi)
                    enemySideSpeed
                    jumpSpeed
                    bounceSpeed
                    gravity
              , ooControlIntentionChanged = newDir /= dir
              }
          )

staticObject :: MovementState -> Object
staticObject ms =
  proc _ -> do
    let triple = ms
    t <- FRP.BearRiver.time -< ()
    let currentTriple = extrapolateMovementState ms t
    returnA
      -<
        ObjOutput
          { ooObsObjState = OOSStaticBlock currentTriple
          , ooRemoveReq = FRP.BearRiver.NoEvent
          , ooControlIntention = defaultControlIntention
          , ooControlIntentionChanged = False
          }

calculateTimeToCollision ::
  MSF
    GameMonad
    ((ControlKeys, IL ObjOutput), IL ObjOutput)
    (FRP.BearRiver.Event (IL Object -> IL Object))
calculateTimeToCollision = proc ((ck, oos1), oos2) -> do
  -- Calculate new blocking data and new trajectories
  (newOutputs, crap) <- arr resolveCollisions'' -< (ck, oos2)
  -- Calculate nearest time to collision
  parC calcTimeToHit
    -<
      [ (oosMovementState <$> elemsIL theRest, oosMovementState oos)
      | obj@(key, oos) <- assocsIL (listToIL newOutputs)
      , let theRest = deleteIL key (listToIL newOutputs)
      ]
  returnA -< crap

-- Calculate new blocking data (i. e. from which sides each object is blocked)
-- And calculate new trajectories based on the new blocking data and changed control intention
resolveCollisions'' ::
  (ControlKeys, IL ObjOutput) ->
  ([ObsObjState], FRP.BearRiver.Event (IL Object -> IL Object))
resolveCollisions'' (ck, oos) = (statesAfterCollisions, switchingEvent)
 where
  statesAfterCollisionsAux = [getObjectStatusesAfterCols k oo | (k, oo) <- assocsIL oos]

  getObjectStatusesAfterCols k oo =
    let os = ooObsObjState oo
        ci = ooControlIntention oo
        ciChanged = ooControlIntentionChanged oo
     in getObjectStatusesAfterCols' k os ci ciChanged

  getObjectStatusesAfterCols' k (OOSStaticBlock ms) ci ciChanged = (OOSStaticBlock ms, k, ci, False)
  getObjectStatusesAfterCols' k oosDynamic ci ciChanged =
    let
      ms = oosMovementState oosDynamic
      blocked = oosBlockingData oosDynamic
      newBlocked = getBlockingData (msRct ms) (rectanglesWithoutK k)
      msNew = getObjectMovementState ms newBlocked ci
      blockChanged = newBlocked /= blocked || ciChanged
     in
      ( oosDynamic{oosMovementState = msNew, oosBlockingData = newBlocked}
      , k
      , ci
      , blockChanged
      )

  statesAfterCollisions = fmap (\(x, _, _, _) -> x) statesAfterCollisionsAux
  switchingEvent = foldl (mergeBy (.)) NoEvent es'
  es' =
    [ FRP.BearRiver.Event $ replaceIL k (newObj oo ci)
    | (oo, k, ci, blockChanged) <- statesAfterCollisionsAux
    , blockChanged
    ]
      ++ [ FRP.BearRiver.Event $ deleteIL k | (k, oo) <- assocsIL oos, ooRemoveReq oo == FRP.BearRiver.Event ()
         ]
  -- TODO: refactor this so that there would be less explicit argument compying for the object state
  newObj ((OOSPlayer ms newBlocked scheme)) ci = playerObject (ms, newBlocked, ci, scheme)
  newObj ((OOSEnemy ms newBlocked leftBound rightBound)) ci = enemyObject (ms, newBlocked, ci, leftBound, rightBound)
  rectanglesWithoutK k = msRct . oosMovementState . ooObsObjState <$> elemsIL (deleteIL k oos)

route :: (ControlKeys, IL ObjOutput) -> IL sf -> IL (ObjInput, sf)
route (ck, objOuts) objs = mapIL routeAux objs
 where
  routeAux (k, obj) =
    ( ObjInput
        { oiControlKeys = ck
        , oiOtherActors = emptyIL
        , oiHit =
            if k `elem` hitByEnemyList ++ hitByPlayerList
              then FRP.BearRiver.Event ()
              else NoEvent
        , oiBounce = if k `elem` bounceList then FRP.BearRiver.Event () else NoEvent
        }
    , obj
    )
  hitByEnemyList =
    map
      fst
      ( filter
          (uncurry hitByEnemyAux)
          (map (Data.Bifunctor.second ooObsObjState) $ assocsIL objOuts)
      )
  hitByPlayerList =
    map
      fst
      ( filter
          (uncurry hitByPlayerAux)
          (map (Data.Bifunctor.second ooObsObjState) $ assocsIL objOuts)
      )
  bounceList =
    map
      fst
      ( filter
          (uncurry canBounceFromEnemyAux)
          (map (Data.Bifunctor.second ooObsObjState) $ assocsIL objOuts)
      )

  hitByEnemyAux k (OOSPlayer ms _ _) = any (isHitByEnemy ms) enemies
  hitByEnemyAux k _ = False

  hitByPlayerAux k (OOSEnemy ms _ _ _) = any (isHitByPlayer ms) players
  hitByPlayerAux k _ = False

  canBounceFromEnemyAux k (OOSPlayer ms blocked _) = any (canBounceFromEnemy ms) enemies
  canBounceFromEnemyAux k _ = False

  enemies = map oosMovementState $ filter isEnemy (map ooObsObjState $ elemsIL objOuts)
  players = map oosMovementState $ filter isPlayer (map ooObsObjState $ elemsIL objOuts)

  canBounceFromEnemy playerState@(MovementState pRct _ _) enemyState@(MovementState eRct _ _) =
    checkAABB (Rct (pX pRct) (pY pRct - epsilon) (w pRct) (h pRct)) eRct

  isHitByEnemy playerState@(MovementState pRct _ _) enemyState@(MovementState eRct _ _) =
    checkAABB (Rct (pX pRct + epsilon) (pY pRct) (w pRct) (h pRct)) eRct
      || checkAABB (Rct (pX pRct - epsilon) (pY pRct) (w pRct) (h pRct)) eRct
      || checkAABB (Rct (pX pRct) (pY pRct + epsilon) (w pRct) (h pRct)) eRct

  isHitByPlayer enemyState@(MovementState eRct _ _) playerState@(MovementState pRct _ _) =
    checkAABB (Rct (pX pRct) (pY pRct - epsilon) (w pRct) (h pRct)) eRct

data LevelChange = LevelUp | LevelRestart

checkForLevelChanges ::
  MSF GameMonad (IL ObjOutput) (FRP.BearRiver.Event LevelChange)
checkForLevelChanges = proc ois -> do
  let numPlayer = length $ filter isPlayer (elemsIL $ mapIL (\(k, x) -> ooObsObjState x) ois)
  let numEnemy = length $ filter isEnemy (elemsIL $ mapIL (\(k, x) -> ooObsObjState x) ois)
  restartEvent <- FRP.BearRiver.iEdge False -< (numPlayer == 0)
  changeLevelEvent <- FRP.BearRiver.iEdge False -< (numEnemy == 0)
  returnA
    -<
      lMerge (restartEvent `tag` LevelRestart) (changeLevelEvent `tag` LevelUp)

game ::
  IL Object ->
  MSF GameMonad ControlKeys (FRP.BearRiver.Event (LevelChange), IL ObjOutput)
game level =
  loopPre
    emptyIL
    ( gameCore level
        >>> ((checkForLevelChanges &&& FRP.BearRiver.identity) &&& FRP.BearRiver.identity)
    )

gameCore ::
  IL Object -> MSF GameMonad (ControlKeys, IL ObjOutput) (IL ObjOutput)
gameCore objs = dpSwitch route objs calculateTimeToCollision (\sfs' f -> gameCore (f sfs'))

multiLevelGame' ::
  [(IL Object, [Text])] -> MSF GameMonad ControlKeys (IL ObjOutput, [Text])
multiLevelGame' [] = proc ck -> do
  returnA
    -<
      ( emptyIL
      , ["Congratulations!", "You have completed all levels!", "Press escape to leave!"]
      )
multiLevelGame' levels@(lvl : lvls) =
  FRP.BearRiver.switch
    ( proc ck -> do
        (changeEv, outs) <- game (fst lvl) -< ck

        returnA -< ((outs, snd lvl), changeEv `tag` newLevels levels changeEv)
    )
    multiLevelGame'
 where
  newLevels [] (FRP.BearRiver.Event LevelUp) = []
  newLevels levels@(l : ls) (FRP.BearRiver.Event LevelUp) = ls
  newLevels levels (FRP.BearRiver.Event LevelRestart) = levels
