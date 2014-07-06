module SWorld where

import SUtils

data World = World {
  hero  :: Coord,
  monster :: Coord,
  wall  :: [Coord],
  ground :: [Coord],
  steps :: Int,
  wMax  :: Coord
} deriving (Show)

modifyWorld :: Input -> World -> World
modifyWorld input world = if legalMove
			    then world{hero = newHeroPos,
			      monster = newMonsterPos}
			    else world
  where heroPos = (hero world)
	newHeroPos = (newPos input heroPos)
	newMonsterPos = moveMonster world
	legalMove = not $ newHeroPos `elem` (wall world)

moveMonster :: World -> Coord
moveMonster w = let heroPos = (hero w)
		    monsterPos = (monster w)
		    vector = subtractCoords heroPos monsterPos
		in if (abs (fst vector)) > (abs (snd vector))
		       then if (fst vector) > 0
			       then addCoords monsterPos (1,  0)
			       else addCoords monsterPos (-1, 0)

		       else if (snd vector) > 0
			       then addCoords monsterPos (0,  1)
			       else addCoords monsterPos (0, -1)

