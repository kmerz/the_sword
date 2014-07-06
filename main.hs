import System.IO (stdin, stdout, BufferMode( NoBuffering),
  hSetEcho, hSetBuffering)
import Prelude hiding (Either(..))
import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper


level = "###########################\n#.........................#\n#...................@.....#\n#.........................#\n#.........................#\n#.........................#\n#.........................#\n#.........................#\n#.........................#\n###########################"

type Coord = (Int, Int)

addCoords :: Coord -> Coord -> Coord
addCoords (a, b) (x, y) = (a + x, b + y)

data World = World {
  hero  :: Coord,
  wall  :: [Coord],
  ground :: [Coord],
  wMax  :: Coord
} deriving (Show)

data Input = Quit | Up | Down | Left | Right deriving (Show, Eq, Ord)

emptyWorld = World {
  wall = [],
  ground = [],
  wMax = (0,0),
  hero = (0,0)
}

fstsnd :: (a,b,c) -> (a,b)
fstsnd (a,b,_) = (a,b)

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
    KeyChar 'q' -> return Quit
    otherwise -> getInput

newPos :: Input -> Coord -> Coord
newPos input coord =
  case input of
      Up	-> addCoords (0, -1) coord
      Down 	-> addCoords (0,  1) coord
      Left 	-> addCoords (-1, 0) coord
      Right 	-> addCoords (1,  0) coord

modifyWorld :: Input -> World -> World
modifyWorld input world = if legalMove
			    then world{hero = newHeroPos}
			    else world
  where heroPos = (hero world)
	newHeroPos = (newPos input heroPos)
	legalMove = not $ newHeroPos `elem` (wall world)

gameLoop :: World -> IO ()
gameLoop world = do
  drawWorld world
  input <- getInput
  if input == Quit then
    return ()
  else
    let world' = (modifyWorld input world)
      in gameLoop world'

castEnum = toEnum . fromEnum

drawWorld :: World -> IO ()
drawWorld world = do
  erase
  sequence (map (\ (x,y) -> mvAddCh y x (castEnum '#')) (wall world))
  sequence (map (\ (x,y) -> mvAddCh y x (castEnum '.')) (ground world))
  mvAddCh (snd (hero world)) (fst (hero world)) (castEnum '@')
  refresh

main :: IO ()
main = do
  initCurses
  echo False
  cursSet CursorInvisible
  (sizeY, sizeX) <- scrSize

  world <- return $ (loadLevel level)
  gameLoop world
  endWin
