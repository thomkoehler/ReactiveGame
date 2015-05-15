
----------------------------------------------------------------------------------------------------

{-# LANGUAGE QuasiQuotes #-}

module Game(allLevels) where

import Data.Array.IArray

import Level

----------------------------------------------------------------------------------------------------

allLevels :: [Level]
allLevels =
   [
      level0
   ]


level0 = [level|
wwww
w  w
wwww
|]

level1 = [level|
wwwwwwwwww
w        w
w    w   w
w        w
wwwwwwwwww
|]

----------------------------------------------------------------------------------------------------
