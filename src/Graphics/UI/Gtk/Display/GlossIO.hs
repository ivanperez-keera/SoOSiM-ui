{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports  #-}
{-# LANGUAGE PatternGuards  #-}
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
import System.Glib.Types
import "gloss-gtk" Graphics.Gloss
import "gloss-gtk" Graphics.Gloss.Interface.IO.Animate
import "gloss-gtk" Graphics.Gloss.Interface.IO.Game as G
import Graphics.Diagrams.Types ( Name, addPos, subPos, unScale )

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

glossIONew :: IO GlossIO
glossIONew = do
  ev    <- eventBoxNew 
  zoomL <- newCBMVar 1
  orig  <- newCBMVar (0,0)
  pic   <- newCBMVar Nothing
  return $ GlossIO ev (GlossIOParams zoomL orig pic)

reposition :: Float -> G.Point -> Picture -> Picture
reposition progScale orig = uncurry translate orig . scale progScale progScale

glossIONewWithAnimation :: (Int, Int) -> (Float -> IO Picture) -> IO GlossIO
glossIONewWithAnimation size f = do
  gloss <- glossIONew 
  glossIOStartAnimation gloss size f
  return gloss

glossIOStartAnimation :: GlossIO -> (Int, Int) -> (Float -> IO Picture) -> IO ()
glossIOStartAnimation gloss size f = void $
  gloss `on` realize $ glossIOStartAnimation' gloss size f
  
glossIOStartAnimation' :: GlossIO -> (Int, Int) -> (Float -> IO Picture) -> IO ()
glossIOStartAnimation' (GlossIO ev params) size f = do
  animateIO (InWidget ev size) white f'
 where f' x = do x' <- f x
                 zoom <- readCBMVar $ glossIOZoom params
                 diff <- readCBMVar $ glossIOTranslation params
                 let picture = reposition zoom diff x'
                 writeCBMVar (glossIOPicture params) $ Just x'
                 return picture

data GameState a = GameState Float G.Point (Maybe G.Point) a

glossIOStartGame :: GlossIO
                 -> Float -> G.Point
                 -> (Int, Int) -> Int -> a -> (a -> IO Picture)
                 -> (Event -> a -> a)
                 -> (Float -> a -> IO a) -> IO ()
glossIOStartGame gloss initZ initO size fps state mkPic queue stepW = do
  let (GlossIO e paramsR) = gloss
  glossIOSetZoom gloss initZ
  glossIOSetOrig gloss initO
  let mkPic' x = do let (GameState _ _ _ x') = x
                    zoom <- glossIOGetZoom gloss
                    diff <- glossIOGetOrig gloss
                    x'' <- mkPic x'
                    let picture = reposition zoom diff x''
                    writeCBMVar (glossIOPicture paramsR) $ Just x''
                    return picture
      state' = GameState initZ initO Nothing state
      queue' = transformEvent queue
      stepW' = stepWorld gloss stepW
      play = playIO (InWidget e size) white fps state' mkPic' queue' stepW'
  gloss `on` realize $ play
  return ()

glossIONewWithGame :: Float -> G.Point
                   -> (Int, Int) -> Int -> a -> (a -> IO Picture)
                   -> (Event -> a -> a)
                   -> (Float -> a -> IO a) -> IO GlossIO
glossIONewWithGame initZ initO size fps state mkPic queue stepW = do
  gloss@(GlossIO e paramsR) <- glossIONew 
  glossIOStartGame gloss initZ initO size fps state mkPic queue stepW
  return gloss
  -- glossIOSetZoom gloss initZ
  -- glossIOSetOrig gloss initO
  -- let mkPic' x = do let (GameState _ _ _ x') = x
  --                   zoom <- glossIOGetZoom gloss
  --                   diff <- glossIOGetOrig gloss
  --                   x'' <- mkPic x'
  --                   let picture = reposition zoom diff x''
  --                   writeCBMVar (glossIOPicture paramsR) $ Just picture
  --                   return picture
  --     state' = GameState initZ initO Nothing state
  --     queue' = transformEvent queue
  --     stepW' = stepWorld gloss stepW
  --     play = playIO (InWidget e size) white fps state' mkPic' queue' stepW'
  -- gloss `on` realize $ play
  -- return gloss

-- Process the event queue and return an empty state
stepWorld :: GlossIO -> (Float -> a -> IO a) -> Float -> (GameState a) -> IO (GameState a)
stepWorld glossIO internalTrans f (GameState sc o no internalSt) = do
  internalSt' <- internalTrans f internalSt
  glossIOSetZoom glossIO sc
  glossIOSetOrig glossIO o
  return (GameState sc o no internalSt')

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
  glossIOOnZoomChange (GlossIO _ (GlossIOParams zoomV _ _)) p = do
    installCallbackCBMVar zoomV p
  
  -- glossIOOnOrigChange :: GlossIO -> IO () -> IO ()
  glossIOOnOrigChange (GlossIO _ (GlossIOParams _ diffV _)) p = do
    installCallbackCBMVar diffV p
