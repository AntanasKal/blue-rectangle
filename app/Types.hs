{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Types where

import DPSwitch (dpSwitch)
import IdentityList

import Control.Monad.Trans.MSF
import Data.Functor.Identity
import Data.MonadicStreamFunction
import FRP.BearRiver (Event (Event, NoEvent))

import qualified Data.Text as T

import qualified Data.Bifunctor
import Data.Text (Text)

type DTime = Double

data FutureTime = AtTime DTime | Infinity
  deriving (Eq, Ord, Show)

instance Semigroup FutureTime where
  (<>) = min

instance Monoid FutureTime where
  mempty :: FutureTime
  mempty = Infinity

type CCDGameMonad m = ReaderT DTime (WriterT FutureTime m)

type GameMonad = (CCDGameMonad Identity)

data Rct = Rct
  { pX :: Double -- x coord of the bottom left corner
  , pY :: Double -- y coord of the bottom left corner
  , w :: Double
  , h :: Double
  }
  deriving (Eq)

data Vct = Vct
  { x :: Double
  , y :: Double
  }
  deriving (Eq)

type Object = MSF GameMonad ObjInput ObjOutput

data ObjInput = ObjInput
  { oiOtherActors :: IL ObjOutput
  , oiControlKeys :: ControlKeys
  , oiHit :: FRP.BearRiver.Event ()
  , oiBounce :: FRP.BearRiver.Event ()
  }

data ObjOutput = ObjOutput
  { ooObsObjState :: ObsObjState
  , ooRemoveReq :: FRP.BearRiver.Event ()
  , ooControlIntention :: ControlIntention
  , ooControlIntentionChanged :: Bool
  }

data GameOutput = GameOutput
  { goObjOutputs :: IL ObjInput
  , goText :: [Text]
  }

data MovementState = MovementState
  { msRct :: Rct
  , msVel :: Vct
  , msAcc :: Vct
  }
  deriving (Eq)

data BlockingData = BlockingData
  { up :: Bool
  , right :: Bool
  , down :: Bool
  , left :: Bool
  , upRight :: Bool
  , downRight :: Bool
  , downLeft :: Bool
  , upLeft :: Bool
  }
  deriving (Eq)

blockingDataDefault :: BlockingData
blockingDataDefault = BlockingData True True True True True True True True

data ControlKeys = ControlKeys
  { keyW :: Bool
  , keyA :: Bool
  , keyS :: Bool
  , keyD :: Bool
  , keyUp :: Bool
  , keyLeft :: Bool
  , keyDown :: Bool
  , keyRight :: Bool
  , keyEnter :: Bool
  , keyEscape :: Bool
  , keySpace :: Bool
  }
  deriving (Eq)

data CtrlScheme = CtrlScheme
  { cUp :: ControlKeys -> Bool
  , cLeft :: ControlKeys -> Bool
  , cDown :: ControlKeys -> Bool
  , cRight :: ControlKeys -> Bool
  }

letterScheme :: CtrlScheme
letterScheme = CtrlScheme keyW keyA keyS keyD

arrowScheme :: CtrlScheme
arrowScheme = CtrlScheme keyUp keyLeft keyDown keyRight

letterSpaceScheme :: CtrlScheme
letterSpaceScheme = CtrlScheme keySpace keyA keyS keyD

controlKeysDefault :: ControlKeys
controlKeysDefault =
  ControlKeys
    { keyW = False
    , keyA = False
    , keyS = False
    , keyD = False
    , keyUp = False
    , keyLeft = False
    , keyDown = False
    , keyRight = False
    , keyEnter = False
    , keyEscape = False
    , keySpace = False
    }

data Direction = DirLeft | DirRight | DirNone
  deriving (Eq)

data ControlIntention = ControlIntention
  { ciDir :: Direction
  , ciJump :: Bool
  , ciBounce :: Bool
  , ciControlKeys :: ControlKeys -- perhaps redundant
  , ciSideVel :: Double
  , ciJumpVel :: Double
  , ciBounceVel :: Double
  , ciGravity :: Double
  }

defaultControlIntention :: ControlIntention
defaultControlIntention =
  ControlIntention
    { ciDir = DirNone
    , ciJump = False
    , ciBounce = False
    , ciControlKeys = controlKeysDefault
    , ciBounceVel = 0
    , ciJumpVel = 0
    , ciGravity = 0
    , ciSideVel = 0
    }

data ObsObjState
  = OOSPlayer
      { oosMovementState :: MovementState
      , oosBlockingData :: BlockingData
      , ctrlScheme :: CtrlScheme
      }
  | OOSEnemy
      { oosMovementState :: MovementState
      , oosBlockingData :: BlockingData
      , oosLeftBound :: Maybe Double
      , oosRightBound :: Maybe Double
      }
  | OOSStaticBlock
      { oosMovementState :: MovementState
      }

isPlayer :: ObsObjState -> Bool
isPlayer (OOSPlayer{}) = True
isPlayer _ = False
isEnemy :: ObsObjState -> Bool
isEnemy (OOSEnemy{}) = True
isEnemy _ = False
isStaticBlock :: ObsObjState -> Bool
isStaticBlock (OOSStaticBlock{}) = True
isStaticBlock _ = False
