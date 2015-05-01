
-----------------------------------------------------------------------------------------------------------------------

module Main(main) where


import qualified Graphics.UI.SDL as SDL

import Data.Word

import Reactive.Banana
import Reactive.Banana.SDL
import Reactive.Banana.SDL.Graphics
import Reactive.Banana.Frameworks (actuate, Frameworks)

import Control.Monad.IO.Class(liftIO)

-----------------------------------------------------------------------------------------------------------------------

width :: Int
width = 640


height :: Int
height =  480


black :: SDL.Color
black = SDL.Color 0 0 0


data GraphicsData = GraphicsData
   {
      gdMainSurf :: SDL.Surface
   }

data GameState = GameState
   {
   }


initialGameState = GameState


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
updateGSOnKey key gameState = gameState


startGraphic :: GameState -> Graphic
startGraphic _ = draw (Fill (Just $ SDL.Rect 0 0 width height) black) (Mask Nothing 0 0)


setupNetwork :: Frameworks t => SDLEventSource -> GraphicsData -> Moment t ()
setupNetwork es gd = do
   eTickDiff <- tickDiffEvent es
   esdl <- sdlEvent es

   let
      bScreen = pure $ gdMainSurf gd
      eGSChange= (updateGS <$> eTickDiff) `union` (updateGSOnKey <$> keyDownEvent esdl)
      bGameState = accumB initialGameState eGSChange

   renderGraph (startGraphic <$> bGameState) bScreen
   return ()


-----------------------------------------------------------------------------------------------------------------------


