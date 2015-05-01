
-----------------------------------------------------------------------------------------------------------------------

module Main(main) where


import qualified Graphics.UI.SDL as SDL

import Reactive.Banana
import Reactive.Banana.SDL
import Reactive.Banana.SDL.Graphics

import Control.Monad.IO.Class(liftIO)

-----------------------------------------------------------------------------------------------------------------------

main = do
   liftIO initGraphics
   endGraphics


initGraphics :: IO ()
initGraphics = do
   SDL.init [SDL.InitEverything]
--   SDL.setVideoMode 800 600 32 []
--   SDL.setCaption "Hallo"


endGraphics :: IO ()
endGraphics = do
   SDL.quit

-----------------------------------------------------------------------------------------------------------------------


