module SWorld where

import SUtils

data Hero = Hero {
  position :: Coord,
  life :: Int
} deriving (Show)

data Monster = Monster {
  mposition :: Coord,
  mlife :: Int,
  lastMove :: Int
} deriving (Show)

data World = World {
  hero  :: Hero,
  monster :: Monster,
  wall  :: [Coord],
  ground :: [Coord],
  steps :: Int,
  wMax  :: Coord
} deriving (Show)

modifyWorld :: Input -> World -> Int -> World
modifyWorld input world now = if legalMove
			      then world{hero = (hero world) {
				   position = newHeroPos }}
			      else world
  where heroPos = position (hero world)
	newHeroPos = newPos input heroPos
	legalMove = not $ newHeroPos `elem` (wall world)

legalMonsterMove :: Coord -> Int -> World -> Bool
legalMonsterMove pos now w = let timediff = now - (lastMove (monster w))
				 legalpos = (not $ pos `elem` (wall w)) &&
					 (not $ pos == (position (hero w)))
			     in legalpos && timediff > 2

moveMonster :: World -> Coord
moveMonster w = let heroPos = (position (hero w))
		    monsterPos = (mposition (monster w))
		    vector = subtractCoords heroPos monsterPos
		in if (abs (fst vector)) > (abs (snd vector))
		       then if (fst vector) > 0
			       then addCoords monsterPos (1,  0)
			       else addCoords monsterPos (-1, 0)
		       else if (snd vector) > 0
			       then addCoords monsterPos (0,  1)
			       else addCoords monsterPos (0, -1)
