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
moveMonster tnow c m w
  | timeToMove && needMove && legalMove = w{monster = newMonsterMap}
  | timeToMove && (not needMove) = makeMonsterHit w c m tnow
  | otherwise = w
  where nextMove = calcMoveMonster w c
	needMove = not $ nextMove == (position (hero w))
        legalMove = legalMonsterMove nextMove m w
	monsterMap = Map.delete c (monster w)
	newMonsterMap = Map.union newMonster monsterMap
	newMonster = Map.fromList [(nextMove, m{lastMove = tnow})]
	timeSinceLastMove = diffClockTimes tnow (lastMove m)
	timeToNextMove = (TimeDiff 0 0 0 0 0 0 500000000000)
        timeToMove = timeSinceLastMove >= timeToNextMove

makeMonsterHit :: World -> Coord -> Monster -> ClockTime -> World
makeMonsterHit w c m tnow = w'
  where w' = w { gamelog = newgamelog, hero = newHero, monster = newMonsterMap}
        newgamelog = "Monster hits you with 1":(gamelog w)
        newHero = (hero w){ life = newLife }
        newLife = if ((life (hero w)) - 1 <= 0) then 0 else (life (hero w)) - 1
	newMonsterMap = Map.insert c m{lastMove = tnow} (monster w)

legalMonsterMove :: Coord -> Monster -> World -> Bool
legalMonsterMove pos m w = legalpos
  where legalpos = notOnWall && notOnHero && notOnMonster
	notOnWall = (not $ pos `elem` (wall w))
	notOnHero = (not $ pos == (position (hero w)))
	notOnMonster = Map.notMember pos (monster w)

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
