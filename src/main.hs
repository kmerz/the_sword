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

emptyHero = Hero {
  position = (0,0),
  life = 0,
  maxLife = 100,
  hit = (None, (0,0))
}

emptyMonster = Monster {
  mposition = (0,0),
  mlife = 5,
  lastMove = TOD 0 0
}

emptyWorld = World {
  gamelog = ["You should move", "Welcome to The Sword"],
  wall = [],
  ground = [],
  steps = 0,
  wMax = (0,0),
  hero = emptyHero,
  monster = []
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
            'x' -> wld{monster = emptyMonster{mposition = c, lastMove = tnow}:
	      (monster wld), ground = c:ground wld}
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
  sequence (map drawMonster (monster world))
  drawHit (hero world)
  drawStats (hero world)
  drawLog (gamelog world) (0, 23)
  refresh
  where drawWall = drawChar '#'
	drawGround = drawChar '.'
        drawMonster (Monster x _ _) = drawChar 'x' x

drawChar :: Char -> Coord -> IO ()
drawChar ' ' _ = return ()
drawChar char (x,y) = mvAddCh y x (castEnum char)

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

drawHit :: Hero -> IO ()
drawHit (Hero _ _ _ (input, a))
  | input `elem` [FightLeft, FightRight] = drawChar '-' a
  | input `elem` [FightUp, FightDown] = drawChar '|' a
  | otherwise = return ()

drawStats :: Hero -> IO ()
drawStats (Hero (x,y) life maxLife _) = do
  (drawString ("@ " ++ show (x, y) ++ " Life: " ++ show life ++ "%") (0, 22))

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