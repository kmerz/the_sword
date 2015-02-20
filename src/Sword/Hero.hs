module Sword.Hero where

import Sword.Utils
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)

data Hero = Hero {
  position :: Coord,
  life :: Int,
  name :: String,
  maxLife :: Int,
  lastMove :: UTCTime,
  hit :: (Input, Coord)
} deriving (Show, Read)

newHeroCoord :: Hero -> Input -> Coord
newHeroCoord Hero {position = pos} input = newPos input pos

timeToMoveHero :: Hero -> UTCTime -> Bool
timeToMoveHero Hero {lastMove=lastTime} tnow = timeToMove
  where timeSinceLastMove = diffUTCTime tnow lastTime
	timeToNextMove = 0.1
	timeToMove = timeSinceLastMove >= timeToNextMove

needHeroMove :: Hero -> Coord -> Bool
needHeroMove Hero {position = pos} newPos = newPos /= pos

moveHero :: Hero -> Coord -> UTCTime -> Hero
moveHero h@(Hero {position = pos}) newPos t = h{position = newPos, lastMove = t}
