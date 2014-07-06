module Main where

import System.IO (stdin, stdout, BufferMode( NoBuffering),
  hSetEcho, hSetBuffering)
import Prelude hiding (Either(..))
import Data.Char
import Control.Concurrent (threadDelay)
import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper

import SUtils
import SWorld

level = "###########################\n#.........................#\n#...................@.....#\n#.........................#\n#.........................#\n#.........................#\n#......................x..#\n#.........................#\n#.........................#\n###########################"

emptyWorld = World {
  wall = [],
  ground = [],
  steps = 0,
  wMax = (0,0),
  hero = (0,0),
  monster = (0,0)
}

loadLevel :: String -> World
loadLevel str = foldl consume (emptyWorld{wMax = maxi}) elems
  where lns     = lines str
        coords  = [[(x,y) | x <- [0..]] | y <- [0..]]
        elems   = concat $ zipWith zip coords lns
        maxX    = maximum . map (fst . fst) $ elems
        maxY    = maximum . map (snd . fst) $ elems
        maxi    = (maxX, maxY)
        consume wld (c, elt) =
          case elt of
            '@' -> wld{hero = c, ground = c:ground wld}
            'x' -> wld{monster = c, ground = c:ground wld}
            '#' -> wld{wall = c:wall wld}
            '.' -> wld{ground = c:ground wld}
            otherwise -> error (show elt ++ " not recognized")

getInput = do
  -- a poor mans timeout :(
  threadDelay 200000
  char <- getch
  case decodeKey char of
    KeyChar 'k' -> return Up
    KeyChar 'j' -> return Down
    KeyChar 'h' -> return Left
    KeyChar 'l' -> return Right
    KeyChar 'q' -> return Quit
    otherwise -> return None

gameLoop :: World -> IO ()
gameLoop world = do
  drawWorld world
  input <- getInput
  case input of
    Quit -> return ()
    otherwise -> let world' = (modifyWorld input world)
                   in gameLoop world'

castEnum = toEnum . fromEnum

drawWorld :: World -> IO ()
drawWorld world = do
  erase
  sequence (map (\ (x,y) -> mvAddCh y x (castEnum '#')) (wall world))
  sequence (map (\ (x,y) -> mvAddCh y x (castEnum '.')) (ground world))
  mvAddCh (snd (hero world)) (fst (hero world)) (castEnum '@')
  mvAddCh (snd (monster world)) (fst (monster world)) (castEnum 'x')
  refresh

main :: IO ()
main = do
  initCurses
  echo False
  noDelay stdScr True
  timeout 1000
  cursSet CursorInvisible
  (sizeY, sizeX) <- scrSize

  world <- return $ (loadLevel level)
  gameLoop world
  endWin
