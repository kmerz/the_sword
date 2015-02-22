module Sword.World where

import qualified Data.Map as Map

import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import Data.Maybe (isJust)
import Sword.Utils
import Sword.Hero

data Monster = Monster {
  mlife :: Int,
  mlastMove :: UTCTime,
  awake :: Bool
} deriving (Show, Read)

data WorldObj = Ground | Wall | Tree deriving(Show, Eq, Ord, Enum, Read)
type WorldMap = Map.Map Coord WorldObj
type Heros = Map.Map Int Hero

data World = World {
  heros :: Heros,
  monster :: Map.Map Coord Monster,
  gamelog :: [String]
} deriving (Show, Read)

emptyHero = Hero{position = (0,0)}

addHero :: String -> Int -> UTCTime -> World -> World
addHero name id tnow world@World{heros = h} = world{heros = newHeros}
  where newHeros = Map.insert id Hero{
    position = (2,id+2),
    life = 10,
    name = name,
    lastMove = tnow,
    maxLife = 100,
    hit = (None, (0,0))} h

nearestHero :: Coord -> Heros -> (Int, Hero)
nearestHero c heros = Map.foldWithKey (findNext c) (0, emptyHero) heros
  where findNext _ id h (0, _) = (id, h)
        findNext c id h acc =
          if (position h) < position (snd acc) && (position h) < c
          then (id, h) else acc

wMax :: WorldMap -> Coord
wMax w = Map.foldWithKey findMax (0,0) w
  where findMax a _ acc = if a > acc then a else acc

legalPos :: WorldMap -> Coord -> Bool
legalPos m c = Map.lookup c m == Just Ground

modifyWorld :: Int -> Input -> UTCTime -> WorldMap -> World -> World
modifyWorld heroId input tnow worldM world = mMove $ (alertMonsters hero) $ hMove world
  where mMove = moveMonsters tnow worldM
        hMove = heroAction heroId hero tnow input worldM
	hero = Map.lookup heroId (heros world)

heroAction :: Int -> Maybe Hero -> UTCTime -> Input -> WorldMap -> World -> World
heroAction _ Nothing _ _ _ w = w
heroAction id (Just h) tnow input worldM world@World{monster = mon}
  | timeToMove && strikeHit = makeHit id h tnow newHeroPos input m world
  | timeToMove && needMove && legalMove = world{heros = Map.insert id newH herosMap}
  | otherwise = world
  where newHeroPos = newHeroCoord h input
	timeToMove = timeToMoveHero h tnow
	needMove = needHeroMove h newHeroPos
	strikeHit = input `elem` fightMoves
        legalMove = legalPos worldM newHeroPos
	m = Map.lookup newHeroPos mon
	newH = moveHero h newHeroPos tnow
	herosMap = (heros world)

makeHit :: Int -> Hero -> UTCTime -> Coord -> Input -> Maybe Monster -> World -> World
makeHit _ _ _ _ _ Nothing w = w { gamelog = "You hit thin air." : gamelog w }
makeHit id h tnow c i (Just m) w@World{heros = heros} = w { heros = newHeros, monster = newMonsterMap, gamelog = glog }
  where monsterMap = Map.delete c (monster w)
        newHero = h { hit = (i, c), lastMove = tnow }
	newHeros = Map.insert id newHero heros
	newMonsterMap = if newMonsterLife <= 0
                        then monsterMap
                        else Map.union newMonster monsterMap
	newMonsterLife = mlife m - 1
	glog = "You hit with 1" : gamelog w
        newMonster = Map.fromList[(c, m{ mlife = newMonsterLife })]

moveMonsters :: UTCTime -> WorldMap -> World -> World
moveMonsters tnow wM w = Map.foldrWithKey (moveMonster tnow wM) w (monster w)

needMonsterMove :: Coord -> Heros -> Bool
needMonsterMove c heros = Map.foldrWithKey (needMove c) True heros
  where needMove c _ hero acc = (c /= (position hero)) && acc

moveMonster :: UTCTime -> WorldMap -> Coord -> Monster ->  World -> World
moveMonster tnow wM c m w@World{heros = h}
  | isAwake && timeToMove && needMove && legalMove = w{monster = newMonsterMap}
  | isAwake && timeToMove && not needMove = makeMonsterHit w c m tnow
  | otherwise = w
  where nextMove = calcMoveMonster w c
	needMove = needMonsterMove nextMove h
	isAwake = awake m
        legalMove = legalMonsterMove nextMove m wM w
	monsterMap = Map.delete c (monster w)
	newMonsterMap = Map.union newMonster monsterMap
	newMonster = Map.fromList [(nextMove, m{mlastMove = tnow})]
	timeSinceLastMove = diffUTCTime tnow (mlastMove m)
	timeToNextMove = 1.0
        timeToMove = timeSinceLastMove >= timeToNextMove

alertMonsters :: Maybe Hero -> World -> World
alertMonsters Nothing w = w
alertMonsters (Just h) w = Map.foldrWithKey (alertMonster heroPos) w (monster w)
  where heroPos = position h
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
makeMonsterHit w@World{heros = hMap} c m tnow = w'
  where w' = w { gamelog = newgamelog, heros = newHeros, monster = newMonsterMap}
        newgamelog = "Monster hits you with 1":gamelog w
	(id, hero) = nearestHero c hMap
	newHeros = Map.insert id newHero hMap
        newHero = hero{ life = newLife }
        newLife = if life hero - 1 <= 0 then 0 else life hero - 1
	newMonsterMap = Map.insert c m{mlastMove = tnow} (monster w)

legalMonsterMove :: Coord -> Monster -> WorldMap -> World -> Bool
legalMonsterMove pos m worldM w = notOnObj && notOnHero && notOnMonster && isAwake
  where notOnObj = legalPos worldM pos
	notOnHero = Map.fold (\h acc -> pos /= (position h) && acc) True hers
	hers = (heros w)
	isAwake = awake m
	notOnMonster = Map.notMember pos (monster w)

calcMoveMonster :: World -> Coord -> Coord
calcMoveMonster w monsterPos
  | abs x > abs y && x > 0 = addCoords monsterPos (1,  0)
  | abs x > abs y && x <= 0 = addCoords monsterPos (-1, 0)
  | abs x <= abs y && y > 0 = addCoords monsterPos (0,  1)
  | abs x <= abs y && y <= 0 = addCoords monsterPos (0, -1)
  where (_, hero) = nearestHero monsterPos (heros  w)
        heroPos = position hero
        vector = subtractCoords heroPos monsterPos
	x = fst vector
	y = snd vector
