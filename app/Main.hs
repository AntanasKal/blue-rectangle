{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import DPSwitch (dpSwitch)
import GameCore
import IdentityList
import Levels
import Types

import Control.Concurrent (threadDelay)
import Control.Monad (guard, when)
import Control.Monad.Trans.MSF (
  ReaderT (runReaderT),
  WriterT (runWriterT),
 )
import qualified Data.Bifunctor
import Data.Functor.Identity (Identity (runIdentity))
import Data.IORef (IORef, readIORef, writeIORef)
import Data.MonadicStreamFunction (MSF)
import Data.MonadicStreamFunction.InternalCore (MSF (MSF, unMSF))
import Data.Text (Text)
import qualified Data.Text as T
import Foreign.C.Types (CInt)
import SDL
import qualified SDL
import SDL.Font (Font, initialize, load, solid)
import SDL.Raw (Rect (Rect), quitRequested, vkLoadLibrary)
import qualified SDL.Raw.Font as SDL.Font
import SDL.Vect ()

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (800, 640)

frameStep :: Double
frameStep = 0.02

microSecsPerSec :: Double
microSecsPerSec = 1e6

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  SDL.Font.initialize

  window <-
    SDL.createWindow
      "Blue Rectangle"
      SDL.defaultWindow{SDL.windowInitialSize = V2 screenWidth screenHeight}
  SDL.showWindow window

  -- This font was selected
  -- https://github.com/theleagueof/orbitron
  font <- SDL.Font.load "assets/font.ttf" 30

  renderer <-
    SDL.createRenderer
      window
      (-1)
      SDL.RendererConfig
        { SDL.rendererType = SDL.AcceleratedRenderer
        , SDL.rendererTargetTexture = False
        }
  screenSurface <- SDL.getWindowSurface window

  SDL.updateWindowSurface window

  runGame renderer window font multiLevelSpec 0
  quitGame window

quitGame :: Window -> IO ()
quitGame window = do
  SDL.destroyWindow window
  SDL.quit

handleSDLEvent :: ControlKeys -> SDL.Event -> ControlKeys
handleSDLEvent ctrl ev =
  case eventPayload ev of
    KeyboardEvent (KeyboardEventData _ _ _ (Keysym ScancodeA _ _)) -> ctrl{keyA = not (keyA ctrl)}
    KeyboardEvent (KeyboardEventData _ _ _ (Keysym ScancodeD _ _)) -> ctrl{keyD = not (keyD ctrl)}
    KeyboardEvent (KeyboardEventData _ _ _ (Keysym ScancodeS _ _)) -> ctrl{keyS = not (keyS ctrl)}
    KeyboardEvent (KeyboardEventData _ _ _ (Keysym ScancodeW _ _)) -> ctrl{keyW = not (keyW ctrl)}
    KeyboardEvent (KeyboardEventData _ _ _ (Keysym ScancodeUp _ _)) -> ctrl{keyUp = not (keyUp ctrl)}
    KeyboardEvent (KeyboardEventData _ _ _ (Keysym ScancodeLeft _ _)) -> ctrl{keyLeft = not (keyLeft ctrl)}
    KeyboardEvent (KeyboardEventData _ _ _ (Keysym ScancodeDown _ _)) -> ctrl{keyS = not (keyDown ctrl)}
    KeyboardEvent (KeyboardEventData _ _ _ (Keysym ScancodeRight _ _)) -> ctrl{keyW = not (keyRight ctrl)}

processInput :: Bool -> IO (ControlKeys, Bool)
processInput _ = do
  quitReq <- quitRequested
  es <- pollEvents
  keyHeld <- getKeyboardState
  let ctrl =
        ControlKeys
          { keyA = keyHeld ScancodeA
          , keyW = keyHeld ScancodeW
          , keyS = keyHeld ScancodeS
          , keyD = keyHeld ScancodeD
          , keyUp = keyHeld ScancodeUp
          , keyLeft = keyHeld ScancodeLeft
          , keyDown = keyHeld ScancodeDown
          , keyRight = keyHeld ScancodeRight
          , keyEnter = keyHeld ScancodeKPEnter
          , keyEscape = keyHeld ScancodeEscape
          , keySpace = keyHeld ScancodeSpace
          }
  pure (ctrl, quitReq)

nextTimeStep :: FutureTime -> Double -> DTime
nextTimeStep Infinity delayTime = delayTime -- frameStep
nextTimeStep (AtTime time) delayTime = min time delayTime -- frameStep

-- Run single step of a MSF and extracts future time (from the writer context in GameMonad)
-- and continuation of MSF for next iterations
singleIteration ::
  MSF GameMonad ControlKeys (IL ObjOutput, [Text]) ->
  ControlKeys ->
  Double ->
  Identity
    ( ((IL ObjOutput, [Text]), MSF GameMonad ControlKeys (IL ObjOutput, [Text]))
    , FutureTime
    )
singleIteration myMSF ctrl t = do
  (out, futureTime) <- runWriterT $ runReaderT (unMSF myMSF ctrl) t
  return (out, futureTime)

-- Take several samples of the game,
-- each time extracting the next calculated future time
-- and decrementing the remaining time
runIterationsForGivenTime ::
  ControlKeys ->
  MSF GameMonad ControlKeys (IL ObjOutput, [Text]) ->
  DTime ->
  DTime ->
  IO
    ( ((IL ObjOutput, [Text]), MSF GameMonad ControlKeys (IL ObjOutput, [Text]))
    , FutureTime
    )
runIterationsForGivenTime ctrl myMSF firstStepTime 0 = do
  return $ runIdentity $ singleIteration myMSF ctrl firstStepTime
runIterationsForGivenTime ctrl myMSF firstStepTime furtherStepsTime = do
  let ((objOut, msf), newFutureTime) = runIdentity $ singleIteration myMSF ctrl firstStepTime
  let (AtTime nextStepTime) = min (AtTime furtherStepsTime) newFutureTime
  let remainingTime = furtherStepsTime - nextStepTime
  runIterationsForGivenTime ctrl msf nextStepTime remainingTime

-- Using simple clock technique
-- 1. Getting time before and after game logic step
-- 2. Running game logic step for fixed amount of time (this does not need to be fixed)
-- 3. Rendering
-- 4. Getting the time at the end of iteration
-- 5. Sleeping for the difference between fixed iteration step and measured time differences (with some lower bound on sleeping time)
-- TODO: restructure this function into something more similar to reactimate loop
runGame ::
  Renderer ->
  Window ->
  Font ->
  MSF GameMonad ControlKeys (IL ObjOutput, [Text]) ->
  DTime ->
  IO ()
runGame rend wind font myMSF t = do
  loopStartTime <- SDL.time
  input@(ctrl, quitReq) <- processInput True

  if keyEscape ctrl || quitReq
    then return ()
    else do
      ((outs, msf), ft) <- runIterationsForGivenTime ctrl myMSF t (frameStep - t)

      renderScene outs rend wind font

      loopEndTime <- SDL.time
      let delayTime = max (frameStep - (loopEndTime - loopStartTime)) 0 -- (frameStep / 1000)
      threadDelay $ fromIntegral $ floor $ delayTime * microSecsPerSec

      runGame rend wind font msf (nextTimeStep ft frameStep)

renderScene :: (IL ObjOutput, [Text]) -> Renderer -> Window -> Font -> IO ()
renderScene (objOut, textArray) rend wind font = do
  SDL.rendererDrawColor rend $= V4 20 30 20 maxBound
  clear rend

  mapM_ (renderObject rend) (snd <$> assocsIL objOut)
  renderText textArray 10 font wind rend
  SDL.present rend

renderText :: [Text] -> CInt -> Font -> SDL.Window -> SDL.Renderer -> IO ()
renderText [] _ _ _ _ = return ()
renderText (x : xs) yOffset font wind rend = do
  text <- SDL.Font.solid font (V4 200 200 200 maxBound) x
  dims@(V2 xDim yDim) <- SDL.surfaceDimensions text
  textTexture <- SDL.createTextureFromSurface rend text
  SDL.freeSurface text
  SDL.copy rend textTexture Nothing (Just $ Rectangle (P $ V2 40 yOffset) dims)
  SDL.destroyTexture textTexture
  renderText xs (yOffset + yDim + 5) font wind rend

renderObject :: Renderer -> ObjOutput -> IO ()
renderObject rend obj = do
  let xc = pX $ msRct $ oosMovementState $ ooObsObjState obj
  let yc = pY $ msRct $ oosMovementState $ ooObsObjState obj
  let wc = fromIntegral $ floor $ w $ msRct $ oosMovementState $ ooObsObjState obj
  let hc = fromIntegral $ floor $ h $ msRct $ oosMovementState $ ooObsObjState obj
  colourAction (ooObsObjState obj)
  SDL.fillRect
    rend
    ( Just $
        SDL.Rectangle
          ( P $ V2 (fromIntegral $ floor xc) (fromIntegral $ -hc + screenHeight - floor yc)
          )
          (V2 wc hc)
    )
 where
  colourAction :: ObsObjState -> IO ()
  colourAction OOSPlayer{} = SDL.rendererDrawColor rend $= V4 20 20 150 maxBound
  colourAction OOSStaticBlock{} = SDL.rendererDrawColor rend $= V4 20 200 200 maxBound
  colourAction OOSEnemy{} = SDL.rendererDrawColor rend $= V4 200 20 20 maxBound
