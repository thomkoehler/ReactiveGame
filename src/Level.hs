
-----------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE PatternSynonyms, MultiParamTypeClasses #-}

module Level(Level(..), level) where

import Data.Array.IArray

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Language.Haskell.TH

import Reactive.Banana.SDL.Graphics.Types
import Graphics.UI.SDL.Rect
import Graphics.UI.SDL.Color
import Reactive.Banana.SDL.Graphics.Util

-----------------------------------------------------------------------------------------------------------------------

pattern Wall = 'w'
pattern Empty = ' '


newtype Level = Level
   {
      lItems :: Array (Int, Int) Char
   }
   deriving(Show)


drawLevel :: Int -> Int -> Level -> Graphic
drawLevel x y level = do
   let fill = Fill { _fillClip = Just (Rect 20 20 100 100), _fillColor = Color 0 0 255}
   draw fill (Mask Nothing 20 20)


instance Draw Level Mask where
   draw level mask = drawLevel (_maskX mask) (_maskY mask) level


level :: QuasiQuoter
level = QuasiQuoter
   {
      quoteExp = loadLevel,
      quotePat = undefined,
      quoteType = undefined,
      quoteDec = undefined
   }


loadLevel :: String -> Q Exp
loadLevel txt = do
   let
      (l:ls) = filter (not . null) $ lines $ filter (/= '\r') txt
      w = toInteger $ length l
      h = toInteger $ length ls + 1
      str = concat (l:ls)
      listArray = mkName "listArray"
      levelCon = mkName "Level"
   return $
      AppE
         (ConE levelCon)
         (AppE
            (AppE
               (VarE listArray)
               (TupE [TupE [LitE (IntegerL 0),LitE (IntegerL 0)],TupE [LitE (IntegerL (w - 1)),LitE (IntegerL (h - 1))]]))
               (LitE (StringL str)))

-----------------------------------------------------------------------------------------------------------------------
