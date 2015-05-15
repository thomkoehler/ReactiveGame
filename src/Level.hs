
-----------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE PatternSynonyms, MultiParamTypeClasses #-}

module Level(Level(..), level) where

import Data.Array.IArray
import Data.List.Split

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Language.Haskell.TH

import Reactive.Banana.SDL.Graphics.Types
import Graphics.UI.SDL.Rect
import Graphics.UI.SDL.Color
import Reactive.Banana.SDL.Graphics.Util

import Debug.Trace

-----------------------------------------------------------------------------------------------------------------------

wallWidth :: Int
wallWidth = 50

wallHeight :: Int
wallHeight = 50

pattern Wall = 'w'
pattern Empty = ' '


newtype Level = Level
   {
      lItems :: Array (Int, Int) Char
   }


instance Show Level where
   show (Level items) =
      let
         (_, (w, _)) = bounds items
      in
         show items ++ "\n" ++ unlines (chunksOf (w + 1) (elems items))


drawLevel :: Int -> Int -> Level -> Graphic
drawLevel posX posY level = foldl1 over $ map mfun $ filter ffun $ assocs $ lItems level
   where
      ffun (_, t) = t == Wall
      mfun ((x, y), _) =
         let
            xx = wallWidth * x + posX
            yy = wallHeight * y + posY
         in
            draw (Fill (Just (Rect xx yy wallWidth wallHeight)) (Color 0 0 255)) (Mask Nothing xx yy)





multiDraw :: [Graphic] -> Graphic
multiDraw = foldl1 over


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
