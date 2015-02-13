module Sword.Gui where

import Control.Monad (when)
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
endGui = endWin

getInput = do
  char <- getch
  case decodeKey char of
    KeyChar 'k' -> return "k"
    KeyChar 'j' -> return "j"
    KeyChar 'h' -> return "h"
    KeyChar 'l' -> return "l"
    KeyChar 'K' -> return "K"
    KeyChar 'J' -> return "J"
    KeyChar 'H' -> return "H"
    KeyChar 'L' -> return "L"
    KeyChar 'q' -> return "quit"
    otherwise -> return ""

castEnum = toEnum . fromEnum

drawWorld :: World -> IO ()
drawWorld world = do
  erase
  sequence_ (Map.foldWithKey (drawObj world) [] (worldMap world))
  drawFunc '@' (position (hero world))
  sequence_ (Map.foldrWithKey drawMonster [] (monster world))
  drawStats (hero world) (viewPort world)
  drawLog (gamelog world) (0, 23)
  refresh
  where drawMonster x _ acc = drawFunc 'x' x : acc
	drawFunc = drawElem (viewPort world)

drawObj :: World -> Coord -> WorldObj ->  [IO ()] -> [IO ()]
drawObj world c obj acc
  | obj == Wall = drawFunc '#' c : acc
  | obj == Tree = drawFunc '4' c : acc
  | obj == Ground = drawFunc '.' c : acc
  | otherwise = drawFunc ' ' c : acc
  where drawFunc = drawElem (viewPort world)

drawElem :: ViewPort -> Char -> Coord -> IO ()
drawElem _ ' ' _ = return ()
drawElem viewPort char coord =
  when (insideViewPort viewPort coord) $
    drawChar char (subtractCoords coord (fst viewPort))

drawChar :: Char -> Coord -> IO ()
drawChar ' ' _ = return ()
drawChar char (x, y) = mvAddCh y x (castEnum char)

drawString :: String -> Coord -> IO ()
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
drawStats (Hero (x,y) life maxLife _ _) viewP =
  drawString ("@ " ++ show (x, y) ++ " Life: " ++ show life ++ "% ViewPort: " ++ show viewP) (0, 22)
