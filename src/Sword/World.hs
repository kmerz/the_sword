module Sword.World where

import qualified Data.Map as Map

import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import Data.Maybe (isJust)
import Sword.Utils
import Sword.Hero
import Sword.ViewPorter

data Monster = Monster {
  mlife :: Int,
  mlastMove :: UTCTime,
  awake :: Bool
} deriving (Show, Read)

data WorldObj = Ground | Wall | Tree deriving(Show, Eq, Ord, Enum, Read)
type WorldMap = Map.Map Coord WorldObj

data World = World {
  hero  :: Hero,
  monster :: Map.Map Coord Monster,
  viewPort :: ViewPort,
  gamelog :: [String]
} deriving (Show, Read)

wMax :: WorldMap -> Coord
wMax w = Map.foldWithKey findMax (0,0) w
  where findMax a _ acc = if a > acc then a else acc

legalPos :: WorldMap -> Coord -> Bool
legalPos m c = Map.lookup c m == Just Ground

modifyWorld :: Input -> UTCTime -> WorldMap -> World -> World
modifyWorld input tnow worldM world = mMove $ alertMonsters $ hMove world
  where mMove = moveMonsters tnow worldM
        hMove = heroAction tnow input worldM

heroAction :: UTCTime -> Input -> WorldMap -> World -> World
heroAction tnow input worldM world@World{hero = h, monster = mon, viewPort = vPort}
  | timeToMove && strikeHit = makeHit tnow newHeroPos input m world
  | timeToMove && needMove && legalMove = world{hero = moveHero h newHeroPos tnow, viewPort = vPort'}
  | otherwise = world { hero = h{hit = (None, (0,0))}}
  where newHeroPos = newHeroCoord h input
	timeToMove = timeToMoveHero h tnow
	needMove = needHeroMove h newHeroPos
	strikeHit = input `elem` fightMoves
	vPort' = updateViewPort vPort newHeroPos (wMax worldM)
        legalMove = legalPos worldM newHeroPos
	m = Map.lookup newHeroPos mon

makeHit :: UTCTime -> Coord -> Input -> Maybe Monster -> World -> World
makeHit _ _ _ Nothing w = w { gamelog = "You hit thin air." : gamelog w }
makeHit tnow c i (Just m) w = w { hero = newHero,
  monster = newMonsterMap,
  gamelog = glog }
  where monsterMap = Map.delete c (monster w)
        newHero = (hero w) { hit = (i, c), lastMove = tnow }
	newMonsterMap = if newMonsterLife <= 0
		       then monsterMap
		       else Map.union newMonster monsterMap
	newMonsterLife = mlife m - 1
	glog = "You hit with 1" : gamelog w
        newMonster = Map.fromList[(c, m{ mlife = newMonsterLife })]

moveMonsters :: UTCTime -> WorldMap -> World -> World
moveMonsters tnow wM w = Map.foldrWithKey (moveMonster tnow wM) w (monster w)

moveMonster :: UTCTime -> WorldMap -> Coord -> Monster ->  World -> World
moveMonster tnow wM c m w
  | timeToMove && needMove && legalMove = w{monster = newMonsterMap}
  | timeToMove && not needMove = makeMonsterHit w c m tnow
  | otherwise = w
  where nextMove = calcMoveMonster w c
	needMove = nextMove /= position (hero w)
        legalMove = legalMonsterMove nextMove m wM w
	monsterMap = Map.delete c (monster w)
	newMonsterMap = Map.union newMonster monsterMap
	newMonster = Map.fromList [(nextMove, m{mlastMove = tnow})]
	timeSinceLastMove = diffUTCTime tnow (mlastMove m)
	timeToNextMove = 1.0
        timeToMove = timeSinceLastMove >= timeToNextMove

alertMonsters :: World -> World
alertMonsters w = Map.foldrWithKey (alertMonster heroPos) w (monster w)
  where heroPos = position (hero w)
	monsters = monster w

alertMonster :: Coord -> Coord -> Monster -> World -> World
alertMonster heroPos monPos mon w
  | not isAwake && wakesUp = newWorld
  | isAwake && not wakesUp = newWorld
  | otherwise = w
  where isAwake = awake mon
	newMon = mon{awake = not isAwake}
	newMonsterMap = Map.insert monPos newMon (monster w)
	newWorld = w{monster = newMonsterMap}
	wakesUp = abs x <= 5 && abs y <= 5
        vector = subtractCoords heroPos monPos
	x = fst vector
	y = snd vector

makeMonsterHit :: World -> Coord -> Monster -> UTCTime -> World
makeMonsterHit w c m tnow = w'
  where w' = w { gamelog = newgamelog, hero = newHero, monster = newMonsterMap}
        newgamelog = "Monster hits you with 1":gamelog w
        newHero = (hero w){ life = newLife }
        newLife = if life (hero w) - 1 <= 0 then 0 else life (hero w) - 1
	newMonsterMap = Map.insert c m{mlastMove = tnow} (monster w)

legalMonsterMove :: Coord -> Monster -> WorldMap -> World -> Bool
legalMonsterMove pos m worldM w = notOnObj && notOnHero && notOnMonster && isAwake
  where notOnObj = legalPos worldM pos
	notOnHero = pos /= position (hero w)
	isAwake = awake m
	notOnMonster = Map.notMember pos (monster w)

calcMoveMonster :: World -> Coord -> Coord
calcMoveMonster w monsterPos
  | abs x > abs y && x > 0 = addCoords monsterPos (1,  0)
  | abs x > abs y && x <= 0 = addCoords monsterPos (-1, 0)
  | abs x <= abs y && y > 0 = addCoords monsterPos (0,  1)
  | abs x <= abs y && y <= 0 = addCoords monsterPos (0, -1)
  where heroPos = position (hero w)
        vector = subtractCoords heroPos monsterPos
	x = fst vector
	y = snd vector
