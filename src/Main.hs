
-----------------------------------------------------------------------------------------------------------------------

module Main(main) where


import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as SDLI

import Data.Word
import Data.Char

import Reactive.Banana
import Reactive.Banana.SDL
import Reactive.Banana.SDL.Graphics
import Reactive.Banana.Frameworks (actuate, Frameworks)

import Control.Monad.IO.Class(liftIO)

import Debug.Trace

-----------------------------------------------------------------------------------------------------------------------

width :: Int
width = 640


height :: Int
height =  480


black :: SDL.Color
black = SDL.Color 0 0 0


red :: SDL.Color
red = SDL.Color 255 0 0


data GraphicsData = GraphicsData
   {
      gdMainSurf :: SDL.Surface
   }

data GraphicsResources = GraphicsResources
   {
      grImage0 :: Image,
      grImageBackground :: Image
   }

data GameState = GameState
   {
      gsPosX :: !Int,
      gsPosY :: !Int,
      gsMoveX :: !Int,
      gsMoveY :: !Int
   }
   deriving Show


initialGameState = GameState 10 20 0 0


main = do
   sdlES <- getSDLEventSource
   gd <- liftIO initGraphics
   let gr = GraphicsResources (Image "images/smiley.png") (Image "images/background.png")
   network <- compile $ setupNetwork sdlES gd gr
   actuate network
   runSDLPump sdlES
   endGraphics


initGraphics :: IO GraphicsData
initGraphics = do
   SDL.init [SDL.InitEverything]
   SDL.setVideoMode width height 32 []
   SDL.setCaption "Reactive Game" ""
   videoSurface <- SDL.getVideoSurface
   return $ GraphicsData videoSurface


endGraphics :: IO ()
endGraphics = do
   SDL.quit


-- | update game state on tick
updateGS :: Word32 -> GameState -> GameState
updateGS tick gs = traceShow gs $ gs { gsPosX = gsPosX gs + gsMoveX gs, gsPosY = gsPosY gs + gsMoveY gs }



-- | update game state on key press
updateGSOnKey :: SDL.Keysym -> GameState -> GameState
updateGSOnKey key gameState =
   case SDL.symKey key of
      SDL.SDLK_a -> gameState { gsMoveX = gsMoveX gameState - 1}
      SDL.SDLK_d -> gameState { gsMoveX = gsMoveX gameState + 1}
      SDL.SDLK_w -> gameState { gsMoveY = gsMoveY gameState - 1}
      SDL.SDLK_y -> gameState { gsMoveY = gsMoveY gameState + 1}
      SDL.SDLK_s -> gameState { gsMoveX = 0, gsMoveY = 0 }
      _ -> gameState


drawGraphic :: GraphicsResources -> GameState -> Graphic
drawGraphic gr (GameState x y _ _) =
   let
      background = draw (grImageBackground gr) (Mask Nothing 0 0)
      image = draw (grImage0 gr) (Mask Nothing x y)
   in
      image `over` background


setupNetwork :: Frameworks t => SDLEventSource -> GraphicsData -> GraphicsResources -> Moment t ()
setupNetwork es gd gr = do
   eTickDiff <- tickDiffEvent es
   esdl <- sdlEvent es

   let
      bScreen = pure $ gdMainSurf gd
      eGSChange= (updateGS <$> eTickDiff) `union` (updateGSOnKey <$> keyDownEvent esdl)
      bGameState = accumB initialGameState eGSChange

   renderGraph (drawGraphic gr<$> bGameState) bScreen
   return ()


-----------------------------------------------------------------------------------------------------------------------


