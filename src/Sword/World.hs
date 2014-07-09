module Sword.World where

import System.Time
import Sword.Utils

data Hero = Hero {
  position :: Coord,
  life :: Int,
  hit :: (Input, Coord)
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

modifyWorld :: Input -> ClockTime -> World -> World
modifyWorld input tnow world = mMove $ hMove $ world
  where mMove = moveMonster tnow
        hMove = moveHero input

moveHero :: Input -> World -> World
moveHero input world
  | needMove && legalMove = world{ hero = (hero world) {
       position = newHeroPos }}
  | strikeHit = world { hero = (hero world) { hit = (input, newHeroHit) }}
  | otherwise = world { hero = (hero world) { hit = (None, (0,0)) }}  
  where heroPos = position (hero world)
        newHeroPos = newPos input heroPos
        newHeroHit = newHit input heroPos
	needMove = not (newHeroPos == heroPos)
	strikeHit = input `elem` fightMoves
        legalMove = not $ newHeroPos `elem` (wall world)

moveMonster :: ClockTime -> World -> World
moveMonster tnow w = if legalMove
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
