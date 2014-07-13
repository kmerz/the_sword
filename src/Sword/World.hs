module Sword.World where

import System.Time
import Sword.Utils

data Hero = Hero {
  position :: Coord,
  life :: Int,
  maxLife :: Int,
  hit :: (Input, Coord)
} deriving (Show)

data Monster = Monster {
  mposition :: Coord,
  mlife :: Int,
  lastMove :: ClockTime
} deriving (Show)

data World = World {
  hero  :: Hero,
  monster :: [Monster],
  wall  :: [Coord],
  ground :: [Coord],
  steps :: Int,
  wMax  :: Coord,
  gamelog :: [[Char]]
} deriving (Show)

findMonsterOnCoord :: Coord -> [Monster] -> Maybe Monster
findMonsterOnCoord c [] = Nothing
findMonsterOnCoord c (m:ms) = if (mposition m) == c
  then Just m
  else findMonsterOnCoord c ms

modifyWorld :: Input -> ClockTime -> World -> World
modifyWorld input tnow world = mMove $ hMove $ world
  where mMove = moveMonsters tnow
        hMove = moveHero input

moveHero :: Input -> World -> World
moveHero input world
  | needMove && legalMove = world{ hero = (hero world) {
       position = newHeroPos }}
  | strikeHit = (makeHit newHeroHit input m world)
  | otherwise = world { hero = (hero world) { hit = (None, (0,0)) }}
  where heroPos = position (hero world)
        newHeroPos = newPos input heroPos
        newHeroHit = newHit input heroPos
	needMove = not (newHeroPos == heroPos)
	strikeHit = input `elem` fightMoves
        legalMove = not $ newHeroPos `elem` (wall world)
	m = findMonsterOnCoord newHeroHit (monster world)

makeHit :: Coord -> Input -> Maybe Monster -> World -> World
makeHit _ _ Nothing w = w
makeHit c i (Just m) w = if legalHit c m
  then w { hero = newHero, monster = newMonster, gamelog = glog }
  else w { gamelog = ("You missed " ++ show c):(gamelog w) }
  where newHero = (hero w) { hit = (i, c) }
	newMonster = if newmlife <= 0 then [] else [m{ mlife = newmlife }]
	newmlife = (mlife m) - 1
	glog = "You hit with 1":(gamelog w)
	legalHit c m = c == (mposition m)

moveMonsters :: ClockTime -> World -> World
moveMonsters tnow w = foldr (moveMonster tnow) w (monster w)

moveMonster :: ClockTime -> Monster -> World -> World
moveMonster tnow m w = if legalMove
  then w{monster = [ m{ mposition = nextMove, lastMove = tnow }]}
  else w
  where nextMove = calcMoveMonster w m
        legalMove = legalMonsterMove nextMove tnow w

legalMonsterMove :: Coord -> ClockTime -> World -> Bool
legalMonsterMove pos tnow w = legalpos && timeSinceLastMove >= timeToNextMove
  where timeSinceLastMove = diffClockTimes tnow (lastMove (head (monster w)))
        legalpos = notOnWall && notOnHero
	notOnWall = (not $ pos `elem` (wall w))
	notOnHero = (not $ pos == (position (hero w)))
	timeToNextMove = (TimeDiff 0 0 0 0 0 0 500000000000)

calcMoveMonster :: World -> Monster -> Coord
calcMoveMonster w m
  | abs x > abs y && x > 0 = addCoords monsterPos (1,  0)
  | abs x > abs y && x <= 0 = addCoords monsterPos (-1, 0)
  | abs x <= abs y && y > 0 = addCoords monsterPos (0,  1)
  | abs x <= abs y && y <= 0 = addCoords monsterPos (0, -1)
  where heroPos = (position (hero w))
	monsterPos = (mposition m)
        vector = subtractCoords heroPos monsterPos
	x = (fst vector)
	y = (snd vector)