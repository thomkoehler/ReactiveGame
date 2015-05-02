
-----------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE PatternSynonyms, DeriveDataTypeable, StandaloneDeriving  #-}

module Level(Level(..), level) where

import Data.Array.IArray
import Data.Data
import Data.Typeable

import Language.Haskell.TH.Quote
import qualified Language.Haskell.TH as TH

import Debug.Trace

-----------------------------------------------------------------------------------------------------------------------

pattern Wall = 'w'
pattern Empty = ' '

newtype Level = Level
   {
      lItems :: Array (Int, Int) Char
   }
   deriving(Data, Typeable, Show)


level :: QuasiQuoter
level = QuasiQuoter
   {
      quoteExp = \txt -> dataToExpQ (const Nothing) (loadLevel txt),
      quotePat = undefined,
      quoteType = undefined,
      quoteDec = undefined
   }


loadLevel :: String -> Level
loadLevel txt =
   let
      (l:ls) = filter (\l -> not (null l)) $ lines txt
      w = length l
      h = length ls + 1
      a = listArray ((0, 0), (w - 1, h - 1)) (concat (l:ls))
   in
      Level { lItems = a }


-----------------------------------------------------------------------------------------------------------------------
