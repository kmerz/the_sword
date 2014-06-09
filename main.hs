import System.IO (stdin, stdout, BufferMode( NoBuffering),
  hSetEcho, hSetBuffering)
import Prelude hiding (Either(..))

level = "###########\n#.........#\n#....@.....#\n#.........#\n###########"

type Coord = (Int, Int)

addCoords :: Coord -> Coord -> Coord
addCoords (a, b) (x, y) = (a + x, b + y)

data World = World {
  hero :: Coord,
  wall :: [Coord]
} deriving (Show)

data Input = Up | Down | Left | Right deriving (Show, Eq, Ord)

emptyWorld = World (0,0) []

getInput :: IO Input
getInput = do
  char <- getChar
  case char of
    'k' -> return Up
    'j' -> return Down
    'h' -> return Left
    'l' -> return Right
    otherwise -> getInput

newPos :: Input -> Coord -> Coord
newPos input coord =
  case input of
      Up	-> addCoords (0,  1) coord
      Down 	-> addCoords (0, -1) coord
      Left 	-> addCoords (1,  0) coord
      Right 	-> addCoords (-1, 0) coord

modifyWorld :: Input -> World -> World
modifyWorld input world = world{hero = newPos(input heroPos)}
  where heroPos = (hero world)

gameLoop :: World -> IO ()
gameLoop world = do
  print world
  input <- getInput
  let world' = modifyWorld(input world)
    in gameLoop world'

main :: IO ()
main = do
  -- Don't show input echo
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  
  world <- return emptyWorld
  gameLoop world
