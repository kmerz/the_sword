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

addInput :: Input -> World -> Coord
addInput input world =
  case input of
      Up	-> addCoords (0,  1) (hero world)
      Down 	-> addCoords (0, -1) (hero world)
      Left 	-> addCoords (1,  0) (hero world)
      Right 	-> addCoords (-1, 0) (hero world)

gameLoop :: World -> IO ()
gameLoop world = do
  input <- getInput
  print $ addInput input world
  let world' = world
    in gameLoop world'

main :: IO ()
main = do
  -- Don't show input echo
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  
  world <- return emptyWorld
  gameLoop world
