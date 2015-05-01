
-----------------------------------------------------------------------------------------------------------------------

module Main(main) where


import qualified Graphics.UI.SDL as SDL

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

data GameState = GameState
   {
      posX :: Int,
      posY :: Int
   }


initialGameState = GameState 10 20


main = do
   sdlES <- getSDLEventSource
   gd <- liftIO initGraphics
   network <- compile $ setupNetwork sdlES gd
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
updateGS tick gameState = gameState



-- | update game state on key press
updateGSOnKey :: SDL.Keysym -> GameState -> GameState
updateGSOnKey key gameState =
   case SDL.symKey key of
      SDL.SDLK_a -> gameState { posX = posX gameState - 1}
      SDL.SDLK_d -> gameState { posX = posX gameState + 1}
      _ -> gameState


drawGraphic :: GameState -> Graphic
drawGraphic (GameState x y) = draw (Fill (Just $ SDL.Rect x y 10 10) red) (Mask Nothing 0 0) `over` draw (Fill (Just $ SDL.Rect 0 0 width height) black) (Mask Nothing 0 0)


setupNetwork :: Frameworks t => SDLEventSource -> GraphicsData -> Moment t ()
setupNetwork es gd = do
   eTickDiff <- tickDiffEvent es
   esdl <- sdlEvent es

   let
      bScreen = pure $ gdMainSurf gd
      eGSChange= (updateGS <$> eTickDiff) `union` (updateGSOnKey <$> keyDownEvent esdl)
      bGameState = accumB initialGameState eGSChange

   renderGraph (drawGraphic <$> bGameState) bScreen
   return ()


-----------------------------------------------------------------------------------------------------------------------


