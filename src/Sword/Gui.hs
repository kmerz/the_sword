module Sword.Gui where

import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper
import qualified Data.Map as Map
import Prelude hiding (Either(..))

import Sword.Utils
import Sword.World
import Sword.Hero

initGui :: IO ()
initGui = do
  initCurses
  echo False
  noDelay stdScr True
  cursSet CursorInvisible
  (sizeY, sizeX) <- scrSize
  return ()

endGui :: IO ()
endGui = do
  endWin

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

castEnum = toEnum . fromEnum

drawWorld :: World -> IO ()
drawWorld world = do
  erase
  sequence (Map.foldWithKey (drawObj $ world) [] (worldMap world))
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

drawObj :: World -> Coord -> WorldObj ->  [IO ()] -> [IO ()]
drawObj world c obj acc
  | obj == Wall = (drawFunc '#' c):acc
  | obj == Tree = (drawFunc '4' c):acc
  | obj == Ground = (drawFunc '.' c):acc
  | otherwise = (drawFunc ' ' c):acc
  where drawFunc = drawElem (viewPort world)

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
