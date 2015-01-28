module Sword.World where

import qualified Data.Map as Map

import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import Sword.Utils
import Sword.Hero

data Monster = Monster {
  mlife :: Int,
  mlastMove :: UTCTime,
  awake :: Bool
} deriving (Show, Read)

data World = World {
  hero  :: Hero,
  monster :: Map.Map Coord Monster,
  wall  :: [Coord],
  ground :: [Coord],
  trees :: [Coord],
  steps :: Int,
  wMax  :: Coord,
  viewPort :: ViewPort,
  gamelog :: [[Char]]
} deriving (Show, Read)

modifyWorld :: Input -> UTCTime -> World -> World
modifyWorld input tnow world = updateViewPort $ mMove $ alertMonsters $ hMove $ world
  where mMove = moveMonsters tnow
        hMove = heroAction tnow input

updateViewPort :: World -> World
updateViewPort w = updateX' $ updateY' w
  where heroPos = (position (hero w))
        viewPortX = (fst (fst (viewPort w)), fst (snd (viewPort w)))
        viewPortY = (snd (fst (viewPort w)), snd (snd (viewPort w)))
        updateX' = updateX (fst heroPos) viewPortX viewPortY
        updateY' = updateY (snd heroPos) viewPortY viewPortX

updateX :: Int -> Coord -> Coord -> World -> World
updateX x (x1, x2) (y1, y2) w
  | (x - x1) <= 30 && x1 > 0 = w{ viewPort = ((x1 - 1, y1), (x2 - 1, y2))}
  | (x2 - x) <= 30 && x2 <= (fst (wMax w)) =
	      w{ viewPort = ((x1 + 1, y1), (x2 + 1, y2))}
  | otherwise = w

updateY :: Int -> Coord -> Coord -> World -> World
updateY y (y1, y2) (x1, x2) w
  | (y - y1) <= 2 && y1 > 0 = w{ viewPort = ((x1, y1 - 1), (x2, y2 - 1))}
  | (y2 - y) <= 2 && y2 <= (snd (wMax w)) =
	      w{ viewPort = ((x1, y1 + 1), (x2, y2 + 1))}
  | otherwise = w

heroAction :: UTCTime -> Input -> World -> World
heroAction tnow input world@World{hero = h, wall = w, monster = mon}
  | timeToMove && strikeHit = (makeHit tnow newHeroPos input m world)
  | timeToMove && needMove && legalMove = world{hero = (moveHero h newHeroPos tnow)}
  | otherwise = world { hero = h{hit = (None, (0,0))}}
  where newHeroPos = newHeroCoord h input
	timeToMove = timeToMoveHero h tnow
	needMove = needHeroMove h newHeroPos
	strikeHit = input `elem` fightMoves
        legalMove = not $ newHeroPos `elem` w
	m = Map.lookup newHeroPos mon

makeHit :: UTCTime -> Coord -> Input -> Maybe Monster -> World -> World
makeHit _ _ _ Nothing w = w { gamelog = ("You hit thin air."):(gamelog w) }
makeHit tnow c i (Just m) w = w { hero = newHero,
  monster = newMonsterMap,
  gamelog = glog }
  where monsterMap = Map.delete c (monster w)
        newHero = (hero w) { hit = (i, c), lastMove = tnow }
	newMonsterMap = if newMonsterLife <= 0
		       then monsterMap
		       else Map.union newMonster monsterMap
	newMonsterLife = (mlife m) - 1
	glog = "You hit with 1":(gamelog w)
        newMonster = Map.fromList[(c, m{ mlife = newMonsterLife })]

moveMonsters :: UTCTime -> World -> World
moveMonsters tnow w = Map.foldrWithKey (moveMonster tnow) w (monster w)

moveMonster :: UTCTime -> Coord -> Monster -> World -> World
moveMonster tnow c m w
  | timeToMove && needMove && legalMove = w{monster = newMonsterMap}
  | timeToMove && (not needMove) = makeMonsterHit w c m tnow
  | otherwise = w
  where nextMove = calcMoveMonster w c
	needMove = not $ nextMove == (position (hero w))
        legalMove = legalMonsterMove nextMove m w
	monsterMap = Map.delete c (monster w)
	newMonsterMap = Map.union newMonster monsterMap
	newMonster = Map.fromList [(nextMove, m{mlastMove = tnow})]
	timeSinceLastMove = diffUTCTime tnow (mlastMove m)
	timeToNextMove = 1.0
        timeToMove = timeSinceLastMove >= timeToNextMove

alertMonsters :: World -> World
alertMonsters w = Map.foldrWithKey (alertMonster heroPos) w (monster w)
  where heroPos = (position (hero w))
	monsters = (monster w)

alertMonster :: Coord -> Coord -> Monster -> World -> World
alertMonster heroPos monPos mon w
  | (not isAwake) && wakesUp = newWorld
  | isAwake && (not wakesUp) = newWorld
  | otherwise = w
  where isAwake = (awake mon)
	newMon = mon{awake = (not isAwake)}
	newMonsterMap = Map.insert monPos newMon (monster w)
	newWorld = w{monster = newMonsterMap}
	wakesUp = abs x <= 5 && abs y <= 5
        vector = subtractCoords heroPos monPos
	x = fst vector
	y = snd vector

makeMonsterHit :: World -> Coord -> Monster -> UTCTime -> World
makeMonsterHit w c m tnow = w'
  where w' = w { gamelog = newgamelog, hero = newHero, monster = newMonsterMap}
        newgamelog = "Monster hits you with 1":(gamelog w)
        newHero = (hero w){ life = newLife }
        newLife = if ((life (hero w)) - 1 <= 0) then 0 else (life (hero w)) - 1
	newMonsterMap = Map.insert c m{mlastMove = tnow} (monster w)

legalMonsterMove :: Coord -> Monster -> World -> Bool
legalMonsterMove pos m w = legalpos
  where legalpos = notOnWall && notOnHero && notOnMonster && isAwake
	notOnWall = (not $ pos `elem` (wall w))
	notOnHero = (not $ pos == (position (hero w)))
	isAwake = (awake m)
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
