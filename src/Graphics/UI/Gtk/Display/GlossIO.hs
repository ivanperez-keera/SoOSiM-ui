{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports  #-}
{-# LANGUAGE PatternGuards  #-}
-- | A Gtk widget that displays a gloss animation/game.
module Graphics.UI.Gtk.Display.GlossIO
    ( glossIONewWithAnimation
    , glossIONewWithGame
    , glossIONew
    , glossIOStartAnimation
    , glossIOStartGame
    , glossIOSetSensitive
    , GlossIO
    , GlossIOClass(..)
    , glossIOGetZoom
    , glossIOGetOrig
    , glossIOGetPicture
    , glossIOAddToOrig
    , glossIOOnZoomChange
    , glossIOOnOrigChange
    )
   where

import Control.Monad
import Data.CBMVar
import Graphics.UI.Gtk (WidgetClass, EventBox, eventBoxNew, ObjectClass, GObjectClass, toGObject, on, realize, eventBoxSetAboveChild)
import Graphics.Zoom
import "gloss-gtk" Graphics.Gloss
import "gloss-gtk" Graphics.Gloss.Interface.IO.Animate
import "gloss-gtk" Graphics.Gloss.Interface.IO.Game as G
import System.Glib.Types

import Graphics.Diagrams.Types ( Name, addPos, subPos, unScale )

-- | A GlossIO Widget displays a gloss animation inside. The animation/game is
-- reactive.
data GlossIO = GlossIO EventBox (GlossIOParams)
data GlossIOParams = GlossIOParams 
  { glossIOZoom        :: CBMVar Float
  , glossIOTranslation :: CBMVar G.Point
  , glossIOPicture     :: CBMVar (Maybe Picture)
  }

instance WidgetClass GlossIO
instance ObjectClass GlossIO
instance GObjectClass GlossIO where
  toGObject (GlossIO e _) = toGObject e
  unsafeCastGObject x = GlossIO (unsafeCastGObject x) undefined

-- | Creates a new empty gloss animation
glossIONew :: IO GlossIO
glossIONew = do
  ev    <- eventBoxNew 
  zoomL <- newCBMVar 1
  orig  <- newCBMVar (0,0)
  pic   <- newCBMVar Nothing
  return $ GlossIO ev (GlossIOParams zoomL orig pic)

-- | Creates a new animation with a given size and an animation generation
-- function (based on time)
glossIONewWithAnimation :: (Int, Int) -> (Float -> IO Picture) -> IO GlossIO
glossIONewWithAnimation size f = do
  gloss <- glossIONew 
  glossIOStartAnimation gloss size f
  return gloss

-- | Starts a new animation inside an empty gloss widget.
--
-- PRE: the gloss widget is not currently playing any animation.
glossIOStartAnimation :: GlossIO -> (Int, Int) -> (Float -> IO Picture) -> IO ()
glossIOStartAnimation gloss size f = void $
  gloss `on` realize $ glossIOStartAnimation' gloss size f

-- Refreshes the gloss animation  
glossIOStartAnimation' :: GlossIO -> (Int, Int) -> (Float -> IO Picture) -> IO ()
glossIOStartAnimation' (GlossIO ev params) size f =
  animateIO (InWidget ev size) white f'
 where f' x = do x' <- f x
                 zoom <- readCBMVar $ glossIOZoom params
                 diff <- readCBMVar $ glossIOTranslation params
                 let picture = reposition zoom diff x'
                 writeCBMVar (glossIOPicture params) $ Just x'
                 return picture

-- The zoom, origin, mouse and other internal state that must be held inside
-- the gloss game for proper interaction
data GameState a = GameState Float G.Point (Maybe G.Point) a

-- | Starts a game inside a gloss widget.
--
-- PRE: no gloss game/animation can be associated to that widget already
glossIOStartGame :: GlossIO
                 -> Float -> G.Point
                 -> (Int, Int) -> Int -> a -> (a -> IO Picture)
                 -> (Event -> a -> a)
                 -> (Float -> a -> IO a) -> IO ()
glossIOStartGame gloss initZ initO size fps state mkPic queue stepW = void $ do
  let (GlossIO e paramsR) = gloss

  -- Set zoom and origin
  glossIOSetZoom gloss initZ
  glossIOSetOrig gloss initO

  -- Define how the game will be presented
  let -- Draw adjusted for zoom and translation
      mkPic' x = do let (GameState _ _ _ x') = x
                    zoom <- glossIOGetZoom gloss
                    diff <- glossIOGetOrig gloss
                    x'' <- mkPic x'
                    let picture = reposition zoom diff x''
                    writeCBMVar (glossIOPicture paramsR) $ Just x''
                    return picture
      -- Initial state
      state' = GameState initZ initO Nothing state
      -- Input event queing function
      queue' = transformEvent queue
      -- Calculate next animation step 
      stepW' = stepWorld gloss stepW
      -- Gloss game
      play = playIO (InWidget e size) white fps state' mkPic' queue' stepW'
  gloss `on` realize $ play

-- | Create a new gloss widget with an embedded game
glossIONewWithGame :: Float -> G.Point
                   -> (Int, Int) -> Int -> a -> (a -> IO Picture)
                   -> (Event -> a -> a)
                   -> (Float -> a -> IO a) -> IO GlossIO
glossIONewWithGame initZ initO size fps state mkPic queue stepW = do
  gloss@(GlossIO e paramsR) <- glossIONew 
  glossIOStartGame gloss initZ initO size fps state mkPic queue stepW
  return gloss

-- Process the event queue and return an empty state
stepWorld :: GlossIO -> (Float -> a -> IO a) -> Float -> GameState a -> IO (GameState a)
stepWorld glossIO internalTrans f (GameState sc o no internalSt) = do
  internalSt' <- internalTrans f internalSt
  glossIOSetZoom glossIO sc
  glossIOSetOrig glossIO o
  return (GameState sc o no internalSt')

-- | Transforms an event relative to the widget into an event relative to the animation
-- (taking zoom and translation into account)
transformEvent ::(Event -> a -> a) -> Event -> GameState a -> GameState a
transformEvent internalTrans event (GameState zoomL orig moving state)
  -- Adds a click event to the event queue
  | EventKey (MouseButton LeftButton) Up m p <- event
  = let p'     = unScale zoomL $ subPos p orig 
        event' = EventKey (MouseButton LeftButton) Up m p'
        state' = internalTrans event' state
    in GameState zoomL orig moving state'

  -- Only Down presses will be received for double clicks
  | EventKey (MouseButton LeftButtonDouble) Down m p <- event
  = let p'     = unScale zoomL $ subPos p orig 
        event' = EventKey (MouseButton LeftButtonDouble) Up m p'
        state' = internalTrans event' state
    in GameState zoomL orig moving state'

  -- Zoom in
  | EventKey (MouseButton WheelUp) Down _ p <- event
  , (sc',o') <- zoomWith stdZoomStep p (zoomL, orig)
  = GameState sc' o' moving state

  -- Zoom out
  | EventKey (MouseButton WheelDown) Down _ p <- event
  , (sc',o') <- zoomWith (1/stdZoomStep) p (zoomL, orig)
  = GameState sc' o' moving state

  -- Start moving
  | EventKey (MouseButton RightButton) Down _ p <- event
  = GameState zoomL orig (Just p) state

  -- Finish moving
  | EventKey (MouseButton RightButton) Up _ _ <- event
  = GameState zoomL orig Nothing state

  -- Keep moving
  | EventMotion p <- event
  , (Just p')     <- moving
  = GameState zoomL (addPos orig (subPos p p')) (Just p) state

  -- Moving over the diagram
  | EventMotion p <- event
  , Nothing       <- moving
  = let p'     = unScale zoomL $ subPos p orig 
        event' = EventMotion p'
        state' = internalTrans event' state
    in GameState zoomL orig Nothing state'

  | otherwise
  = GameState zoomL orig moving (internalTrans event state)

stdZoomStep :: Float
stdZoomStep = 0.8

-- | Gloss Widget class definition
class WidgetClass a => GlossIOClass a where
 glossIOSetSensitive :: a -> Bool -> IO ()
 glossIOSetZoom :: a -> Float -> IO ()
 glossIOGetZoom :: a -> IO Float
 glossIOSetOrig :: a -> G.Point -> IO ()
 glossIOAddToOrig :: a -> G.Point -> IO ()
 glossIOGetOrig :: a -> IO G.Point
 glossIOGetPicture :: a -> IO (Maybe Picture)
 glossIOOnZoomChange :: a -> IO () -> IO ()
 glossIOOnOrigChange :: a -> IO () -> IO ()

-- | To be used for widget derivations
instance GlossIOClass GlossIO where
  -- glossIOSetSensitive :: GlossIO -> Bool -> IO ()
  glossIOSetSensitive (GlossIO ev _) sensitive =
    eventBoxSetAboveChild ev (not sensitive)
  
  -- glossIOSetZoom :: GlossIO -> Float -> IO ()
  glossIOSetZoom (GlossIO _ (GlossIOParams zoomV _ _)) zoom' = do
    zoom <- readCBMVar zoomV
    when (zoom /= zoom') $ writeCBMVar zoomV zoom'
  
  -- glossIOGetZoom :: GlossIO -> IO Float
  glossIOGetZoom (GlossIO _ params) = readCBMVar (glossIOZoom params)
  
  -- glossIOSetOrig :: GlossIO -> G.Point -> IO ()
  glossIOSetOrig (GlossIO _ (GlossIOParams _ diffV _)) point = do
    diff <- readCBMVar diffV
    when (diff /= point) $ writeCBMVar diffV point
  
  -- glossIOAddToOrig :: GlossIO -> G.Point -> IO ()
  glossIOAddToOrig glossIO point = do
    p <- glossIOGetOrig glossIO
    let pAddition = (fst point + fst p, snd point + snd p)
    glossIOSetOrig glossIO pAddition
  
  -- glossIOGetOrig :: GlossIO -> IO G.Point
  glossIOGetOrig (GlossIO _ params) = readCBMVar (glossIOTranslation params)
  
  -- glossIOGetPicture :: GlossIO -> IO (Maybe Picture)
  glossIOGetPicture (GlossIO _ params) = readCBMVar (glossIOPicture params)
  
  -- glossIOOnZoomChange :: GlossIO -> IO () -> IO ()
  glossIOOnZoomChange (GlossIO _ (GlossIOParams zoomV _ _)) p =
    installCallbackCBMVar zoomV p
  
  -- glossIOOnOrigChange :: GlossIO -> IO () -> IO ()
  glossIOOnOrigChange (GlossIO _ (GlossIOParams _ diffV _)) p =
    installCallbackCBMVar diffV p

-- | Reposition a picture based on a zoom level and translation origin
reposition :: Float -> G.Point -> Picture -> Picture
reposition progScale orig = uncurry translate orig . scale progScale progScale
