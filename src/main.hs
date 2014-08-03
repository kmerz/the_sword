module Main where

import System.IO (stdin, stdout, BufferMode( NoBuffering),
  hSetEcho, hSetBuffering)
import Prelude hiding (Either(..))
import Data.Char
import Control.Concurrent (threadDelay)
import System.Time
import qualified Data.Map as Map

import Sword.Utils
import Sword.World
import Sword.Gui

emptyHero = Hero {
  position = (0,0),
  life = 10,
  maxLife = 100,
  hit = (None, (0,0)),
  lastMove = TOD 0 0
}

emptyMonster = Monster {
  mlife = 5,
  mlastMove = TOD 0 0,
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

loadLevel :: String -> ClockTime -> World
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
  input <- getInput
  tnow <- getClockTime
  if input == Quit  || (life (hero (world))) <= 0
    then return ()
    else gameLoop $ modifyWorld input tnow world

main :: IO ()
main = do
  initGui
  timeNow <- getClockTime
  level <- readFile "src/levels/0A_level.txt"
  world <- return $ (loadLevel level timeNow)
  gameLoop world
  endGui
