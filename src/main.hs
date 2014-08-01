module Main where

import System.IO (stdin, stdout, BufferMode( NoBuffering),
  hSetEcho, hSetBuffering)
import Prelude hiding (Either(..))
import Data.Char
import Control.Concurrent (threadDelay)
import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper
import System.Time
import qualified Data.Map as Map

import Sword.Utils
import Sword.World

level = "###############################################################################################################################\n#............x................................................................................................................#\n#...................@.........................................................................................................#\n#.............................................................................................................................#\n#.............................................................................................................................#\n#.......................................................44444444..............................................................#\n#......................x...............................4444444444.............................................................#\n#.....................................................444444444444............................................................#\n#.....................................................4444444444..............................................................#\n#.....................................................4444444444..............................................................#\n#.....................................................4444444444..............................................................#\n#......................................................444444.................................................................#\n#.............................................................................................................................#\n#.............................................................................................................................#\n#.............................................................................................................................#\n#............................................................4................................................................#\n#...........................................................44444.............................................................#\n#.........................................................44444444444.........................................................#\n#.........................................................44444444444.........................................................#\n#............................................................4444.444.........................................................#\n#.................................................................44..........................................................#\n#.............................................................................................................................#\n#.............................................................................................................................#\n#.............................................................................................................................#\n#.............................................................................................................................#\n###############################################################################################################################"

emptyHero = Hero {
  position = (0,0),
  life = 10,
  maxLife = 100,
  hit = (None, (0,0)),
  lastMove = TOD 0 0
}

emptyMonster = Monster {
  mlife = 5,
  mlastMove = TOD 0 0
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

getInput = do
  char <- getch
  case decodeKey char of
    KeyChar 'k' -> return Up
    KeyChar 'j' -> return Down
    KeyChar 'h' -> return Left
    KeyChar 'l' -> return Right
    KeyChar 'K' -> return FightUp
    KeyChar 'J' -> return FightDown
    KeyChar 'H' -> return FightLeft
    KeyChar 'L' -> return FightRight
    KeyChar 'q' -> return Quit
    otherwise -> return None

gameLoop :: World -> IO ()
gameLoop world = do
  drawWorld world
  input <- getInput
  tnow <- getClockTime
  if input == Quit  || (life (hero (world))) <= 0
    then return ()
    else gameLoop $ modifyWorld input tnow world

castEnum = toEnum . fromEnum

drawWorld :: World -> IO ()
drawWorld world = do
  erase
  sequence (map drawWall (wall world))
  sequence (map drawGround (ground world))
  sequence (map drawTrees (trees world))
  drawFunc '@' (position (hero world))
  sequence (Map.foldrWithKey drawMonster [] (monster world))
  drawStats (hero world) (viewPort world)
  drawLog (gamelog world) (0, 23)
  refresh
  where drawWall = drawFunc '#'
	drawGround = drawFunc '.'
	drawTrees = drawFunc '4'
        drawMonster x _ acc = (drawFunc 'x' x):acc
	drawFunc = drawElem (viewPort world)

drawElem :: ViewPort -> Char -> Coord -> IO ()
drawElem _ ' ' _ = return ()
drawElem viewPort char coord = if insideViewPort viewPort coord
  then drawChar char (subtractCoords coord (fst viewPort ))
  else return ()

drawChar :: Char -> Coord -> IO ()
drawChar ' ' _ = return ()
drawChar char (x, y) = mvAddCh y x (castEnum char)

drawString :: [Char] -> Coord -> IO ()
drawString [] _ = return ()
drawString (x:xs) (a, b) = do
  drawChar x (a, b)
  drawString xs (a + 1, b)

drawLog :: [String] -> Coord -> IO ()
drawLog [] _ = return ()
drawLog (x:xs) (a ,b) = do
  drawString x (a, b)
  drawLog xs (0, b + 1)

drawStats :: Hero -> ViewPort -> IO ()
drawStats (Hero (x,y) life maxLife _ _) viewP = do
  (drawString ("@ " ++ show (x, y) ++ " Life: " ++ show life ++ "% ViewPort: " ++ show viewP) (0, 22))

main :: IO ()
main = do
  initCurses
  echo False
  noDelay stdScr True
  cursSet CursorInvisible
  (sizeY, sizeX) <- scrSize

  timeNow <- getClockTime
  world <- return $ (loadLevel level timeNow)
  gameLoop world
  endWin
