module Main where

import System.IO (stdin, stdout, BufferMode( NoBuffering),
  hSetEcho, hSetBuffering)
import Prelude hiding (Either(..))
import Data.Char
import Control.Concurrent (threadDelay)
import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper
import System.Time

import Sword.Utils
import Sword.World

level = "###########################\n#.........................#\n#...................@.....#\n#.........................#\n#.........................#\n#.........................#\n#......................x..#\n#.........................#\n#.........................#\n###########################"

emptyWorld = World {
  wall = [],
  ground = [],
  steps = 0,
  wMax = (0,0),
  hero = Hero { position = (0,0), life = 0, hit = (None, (0,0)) },
  monster = Monster { mposition = (0,0), mlife = 0, lastMove = TOD 0 0}
}

loadLevel :: String -> ClockTime -> World
loadLevel str tnow = foldl consume (emptyWorld{wMax = maxi}) elems
  where lns     = lines str
        coords  = [[(x,y) | x <- [0..]] | y <- [1..]]
        elems   = concat $ zipWith zip coords lns
        maxX    = maximum . map (fst . fst) $ elems
        maxY    = maximum . map (snd . fst) $ elems
        maxi    = (maxX, maxY)
        consume wld (c, elt) =
          case elt of
            '@' -> wld{hero = (hero wld){ position = c },
	      ground = c:ground wld}
            'x' -> wld{monster = (monster wld) {mposition = c,
	      lastMove = tnow}, ground = c:ground wld}
            '#' -> wld{wall = c:wall wld}
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
  case input of
    Quit -> return ()
    otherwise -> gameLoop $ modifyWorld input tnow world

castEnum = toEnum . fromEnum

drawWorld :: World -> IO ()
drawWorld world = do
  erase
  sequence (map drawWall (wall world))
  sequence (map drawGround (ground world))
  drawChar '@' (position (hero world))
  drawChar 'x' (mposition (monster world))
  drawHit (hero world)
  refresh
  where drawWall = drawChar '#'
	drawGround = drawChar '.'

drawChar :: Char -> Coord -> IO ()
drawChar ' ' _ = return ()
drawChar char (x,y) = mvAddCh y x (castEnum char)

drawHit :: Hero -> IO ()
drawHit (Hero _ _ (input, a)) 
  | input `elem` [FightLeft, FightRight] = drawChar '-' a
  | input `elem` [FightUp, FightDown] = drawChar '|' a
  | otherwise = return ()

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
