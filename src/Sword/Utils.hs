module Sword.Utils where

import Prelude hiding (Either(..))

data Input = Quit
	| Up
       	| Down
	| Left
	| Right
	| FightUp
	| FightDown
	| FightLeft
	| FightRight
	| None deriving (Show, Eq, Ord, Read)

type Coord = (Int, Int)

fightMoves = [FightUp, FightDown, FightLeft, FightRight]

subtractCoords :: Coord -> Coord -> Coord
subtractCoords (x,y) (a,b) = (x-a,y-b)

addCoords :: Coord -> Coord -> Coord
addCoords (a, b) (x, y) = (a + x, b + y)

fstsnd :: (a,b,c) -> (a,b)
fstsnd (a,b,_) = (a,b)

newPos :: Input -> Coord -> Coord
newPos input coord =
  case input of
      Up	 -> addCoords (0, -1) coord
      Down 	 -> addCoords (0,  1) coord
      Left 	 -> addCoords (-1, 0) coord
      Right 	 -> addCoords (1,  0) coord
      FightUp	 -> addCoords (0, -1) coord
      FightDown  -> addCoords (0,  1) coord
      FightLeft  -> addCoords (-1, 0) coord
      FightRight -> addCoords (1,  0) coord
      otherwise	 -> coord
