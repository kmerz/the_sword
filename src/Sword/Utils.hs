module Sword.Utils where

import Prelude hiding (Either(..))

data Input = Quit
	| Up
       	| Down
	| Left
	| Right
	| None deriving (Show, Eq, Ord)

type Coord = (Int, Int)

subtractCoords :: Coord -> Coord -> Coord
subtractCoords (x,y) (a,b) = (x-a,y-b)

addCoords :: Coord -> Coord -> Coord
addCoords (a, b) (x, y) = (a + x, b + y)

fstsnd :: (a,b,c) -> (a,b)
fstsnd (a,b,_) = (a,b)

newPos :: Input -> Coord -> Coord
newPos input coord =
  case input of
      Up	-> addCoords (0, -1) coord
      Down 	-> addCoords (0,  1) coord
      Left 	-> addCoords (-1, 0) coord
      Right 	-> addCoords (1,  0) coord
      None	-> coord
