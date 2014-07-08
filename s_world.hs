module SWorld where

import System.Time
import SUtils

data Hero = Hero {
  position :: Coord,
  life :: Int
} deriving (Show)

data Monster = Monster {
  mposition :: Coord,
  mlife :: Int,
  lastMove :: ClockTime
} deriving (Show)

data World = World {
  hero  :: Hero,
  monster :: Monster,
  wall  :: [Coord],
  ground :: [Coord],
  steps :: Int,
  wMax  :: Coord
} deriving (Show)

moveHero :: World -> Input -> World
moveHero world input = if legalMove
			      then world{hero = (hero world) {
				   position = newHeroPos }}
			      else world
  where heroPos = position (hero world)
	newHeroPos = newPos input heroPos
	legalMove = not $ newHeroPos `elem` (wall world)

moveMonster :: World -> ClockTime -> World
moveMonster w tnow = let nextMove = calcMoveMonster w
			 legalMove = legalMonsterMove nextMove tnow w
	             in if legalMove
			  then w{monster = (monster w){ mposition = nextMove,
			         lastMove = tnow}}
			  else w


legalMonsterMove :: Coord -> ClockTime -> World -> Bool
legalMonsterMove pos tnow w = let timediff = diffClockTimes tnow (lastMove (monster w))
				  legalpos = (not $ pos `elem` (wall w)) &&
					     (not $ pos == (position (hero w)))
			     in legalpos && timediff > (TimeDiff 0 0 0 0 0 0 500000000000)

calcMoveMonster :: World -> Coord
calcMoveMonster w = let heroPos = (position (hero w))
		        monsterPos = (mposition (monster w))
		        vector = subtractCoords heroPos monsterPos
		in if (abs (fst vector)) > (abs (snd vector))
		       then if (fst vector) > 0
			       then addCoords monsterPos (1,  0)
			       else addCoords monsterPos (-1, 0)
		       else if (snd vector) > 0
			       then addCoords monsterPos (0,  1)
			       else addCoords monsterPos (0, -1)
