module Sword.World where

import System.Time
import Sword.Utils

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
  then world{hero = (hero world) { position = newHeroPos }}
  else world
  where heroPos = position (hero world)
        newHeroPos = newPos input heroPos
        legalMove = not $ newHeroPos `elem` (wall world)

moveMonster :: World -> ClockTime -> World
moveMonster w tnow = if legalMove
  then w{monster = (monster w){ mposition = nextMove, lastMove = tnow }}
  else w
  where nextMove = calcMoveMonster w
        legalMove = legalMonsterMove nextMove tnow w

legalMonsterMove :: Coord -> ClockTime -> World -> Bool
legalMonsterMove pos tnow w = legalpos && timeSinceLastMove >= timeToNextMove
  where timeSinceLastMove = diffClockTimes tnow (lastMove (monster w))
        legalpos = notOnWall && notOnHero
	notOnWall = (not $ pos `elem` (wall w))
	notOnHero = (not $ pos == (position (hero w)))
	timeToNextMove = (TimeDiff 0 0 0 0 0 0 500000000000)

calcMoveMonster :: World -> Coord
calcMoveMonster w
  | abs x > abs y && x > 0 = addCoords monsterPos (1,  0)
  | abs x > abs y && x <= 0 = addCoords monsterPos (-1, 0)
  | abs x <= abs y && y > 0 = addCoords monsterPos (0,  1)
  | abs x <= abs y && y <= 0 = addCoords monsterPos (0, -1)
  where heroPos = (position (hero w))
	monsterPos = (mposition (monster w))
        vector = subtractCoords heroPos monsterPos
	x = (fst vector)
	y = (snd vector)
