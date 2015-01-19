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
type ViewPort = (Coord, Coord)

fightMoves = [FightUp, FightDown, FightLeft, FightRight]

subtractCoords :: Coord -> Coord -> Coord
subtractCoords (x,y) (a,b) = (x-a,y-b)

addCoords :: Coord -> Coord -> Coord
addCoords (a, b) (x, y) = (a + x, b + y)

fstsnd :: (a,b,c) -> (a,b)
fstsnd (a,b,_) = (a,b)

insideViewPort :: ViewPort -> Coord -> Bool
insideViewPort ((x1, y1), (x2, y2)) (x, y) = xinside && yinside
  where xinside = (x1 <= x) && (x < x2)
	yinside = (y1 <= y) && (y < y2)

newPos :: Input -> Coord -> Coord
newPos input coord =
  case input of
      Up	-> addCoords (0, -1) coord
      Down 	-> addCoords (0,  1) coord
      Left 	-> addCoords (-1, 0) coord
      Right 	-> addCoords (1,  0) coord
      otherwise	-> coord

newHit :: Input -> Coord -> Coord
newHit input coord =
  case input of
      FightUp		-> addCoords (0, -1) coord
      FightDown 	-> addCoords (0,  1) coord
      FightLeft 	-> addCoords (-1, 0) coord
      FightRight 	-> addCoords (1,  0) coord
      otherwise		-> coord
