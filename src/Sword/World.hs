module Sword.World where

import qualified Data.Map as Map

import System.Time
import Sword.Utils

data Hero = Hero {
  position :: Coord,
  life :: Int,
  maxLife :: Int,
  hit :: (Input, Coord)
} deriving (Show)

data Monster = Monster {
  mlife :: Int,
  lastMove :: ClockTime
} deriving (Show)

data World = World {
  hero  :: Hero,
  monster :: Map.Map Coord Monster,
  wall  :: [Coord],
  ground :: [Coord],
  steps :: Int,
  wMax  :: Coord,
  gamelog :: [[Char]]
} deriving (Show)

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
	m = Map.lookup newHeroHit (monster world)

makeHit :: Coord -> Input -> Maybe Monster -> World -> World
makeHit _ _ Nothing w = w { gamelog = ("You hit thin air."):(gamelog w) }
makeHit c i (Just m) w = w { hero = newHero,
  monster = newMonsterMap,
  gamelog = glog }
  where monsterMap = Map.delete c (monster w)
        newHero = (hero w) { hit = (i, c) }
	newMonsterMap = if newMonsterLife <= 0
		       then monsterMap
		       else Map.union newMonster monsterMap
	newMonsterLife = (mlife m) - 1
	glog = "You hit with 1":(gamelog w)
        newMonster = Map.fromList[(c, m{ mlife = newMonsterLife })]

moveMonsters :: ClockTime -> World -> World
moveMonsters tnow w = Map.foldrWithKey (moveMonster tnow) w (monster w)

moveMonster :: ClockTime -> Coord -> Monster -> World -> World
moveMonster tnow c m w = if legalMove
  then w{monster = newMonsterMap}
  else w
  where nextMove = calcMoveMonster w c
        legalMove = legalMonsterMove nextMove tnow m w
	monsterMap = Map.delete c (monster w)
	newMonsterMap = Map.union newMonster monsterMap 
	newMonster = Map.fromList [(nextMove, m{lastMove = tnow})]

legalMonsterMove :: Coord -> ClockTime -> Monster -> World -> Bool
legalMonsterMove pos tnow m w = legalpos && timeSinceLastMove >= timeToNextMove
  where timeSinceLastMove = diffClockTimes tnow (lastMove m)
        legalpos = notOnWall && notOnHero && notOnMonster
	notOnWall = (not $ pos `elem` (wall w))
	notOnHero = (not $ pos == (position (hero w)))
	notOnMonster = Map.notMember pos (monster w)
	timeToNextMove = (TimeDiff 0 0 0 0 0 0 500000000000)

calcMoveMonster :: World -> Coord -> Coord
calcMoveMonster w monsterPos
  | abs x > abs y && x > 0 = addCoords monsterPos (1,  0)
  | abs x > abs y && x <= 0 = addCoords monsterPos (-1, 0)
  | abs x <= abs y && y > 0 = addCoords monsterPos (0,  1)
  | abs x <= abs y && y <= 0 = addCoords monsterPos (0, -1)
  where heroPos = (position (hero w))
        vector = subtractCoords heroPos monsterPos
	x = (fst vector)
	y = (snd vector)
