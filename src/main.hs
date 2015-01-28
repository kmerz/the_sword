module Main where

import Network hiding (accept)
import Network.Socket
import System.Environment (getArgs)
import System.IO
import Prelude hiding (Either(..))
import Data.Char
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Fix (fix)
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import qualified Data.Map as Map

import Sword.Utils
import Sword.World
import Sword.Hero
import Sword.Gui

emptyHero = Hero {
  position = (0,0),
  life = 10,
  maxLife = 100,
  hit = (None, (0,0))
}

emptyMonster = Monster {
  mlife = 5,
  awake = False
}

emptyWorld = World {
  gamelog = ["You should move.", "Welcome to The Sword"],
  wall = [],
  ground = [],
  trees = [],
  steps = 0,
  wMax = (0,0),
  hero = emptyHero,
  viewPort = ((10,0),(90,20)),
  monster = Map.empty
}

loadLevel :: String -> UTCTime -> World
loadLevel str tnow = foldl consume (emptyWorld{wMax = maxi}) elems
  where lns     = lines str
        coords  = [[(x,y) | x <- [0..]] | y <- [0..]]
        elems   = concat $ zipWith zip coords lns
        maxX    = maximum . map (fst . fst) $ elems
        maxY    = maximum . map (snd . fst) $ elems
        maxi    = (maxX, maxY)
        consume wld (c, elt) =
          case elt of
            '@' -> wld{hero = (hero wld){ position = c, lastMove = tnow },
	      ground = c:ground wld}
            'x' -> wld{monster = Map.insert c emptyMonster{
		    mlastMove = tnow} (monster wld), ground = c:ground wld}
            '#' -> wld{wall = c:wall wld}
            '4' -> wld{trees = c:trees wld}
            '.' -> wld{ground = c:ground wld}
            otherwise -> error (show elt ++ " not recognized")

gameLoop :: World -> IO ()
gameLoop world = do
  drawWorld world
  tnow <- getCurrentTime
  input <- getInput
  if input == Quit  || (life (hero (world))) <= 0
    then return ()
    else gameLoop $ modifyWorld input tnow world

main :: IO ()
main = do
  initGui
  timeNow <- getCurrentTime
  level <- readFile "src/levels/0A_level.txt"
  world <- return $ (loadLevel level timeNow)
  gameLoop world
  endGui
