module Sword.Daemon where

import Prelude hiding (Either(..))
import qualified Data.Map as Map
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Fix (fix)

import Sword.Utils
import Sword.World
import Sword.Hero
import Sword.Gui

type Msg = (Int, String)

daemonStart :: IO ()
daemonStart = do
  timeNow <- getCurrentTime
  level <- readFile "src/levels/0A_level.txt"
  world <- return $ (loadLevel level timeNow)
  chan <- newChan
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bindSocket sock (SockAddrInet 4242 iNADDR_ANY)
  -- allow a maximum of 2 outstanding connections
  listen sock 2
  forkIO (daemonGameLoop chan world)
  daemonAcceptLoop sock chan 1

daemonGameLoop :: Chan Msg -> World -> IO ()
daemonGameLoop chan world = do
  (nr, input) <- readChan chan
  case nr of
    0 -> daemonGameLoop chan world --skip message from ourself
    otherwise -> do
      tnow <- getCurrentTime
      newWorld <- return (modifyWorld (convertInput input) tnow world)
      writeChan chan (0, "newWorld")
      daemonGameLoop chan newWorld

daemonAcceptLoop :: Socket -> Chan Msg -> Int -> IO ()
daemonAcceptLoop sock chan nr = do
  conn <- accept sock
  forkIO (runConn conn chan nr)
  daemonAcceptLoop sock chan $! nr + 1

runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, _) chan nr = do
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering
  name <- liftM init (hGetLine hdl)
  --putStrLn ("--> " ++ name ++ " entered game")
  --broadcast ("--> " ++ name ++ " entered.")
  chan' <- dupChan chan
  reader <- forkIO $ fix $ \loop -> do
    (nr', line) <- readChan chan'
    putStrLn ((show nr) ++ ": msg arrived from:" ++ (show nr'))
    when (nr' == 0) $ hPutStrLn hdl line
    loop
  handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
    line <- liftM init (hGetLine hdl)
    case line of
      "quit" -> hPutStrLn hdl "Bye!"
      _ -> do
        writeChan chan (nr, line)
	loop
    killThread reader
    hClose hdl
    loop

emptyHero = Hero {
  position = (3,5),
  life = 10,
  maxLife = 100,
  hit = (None, (0,0))
}

emptyMonster = Monster {
  mlife = 5,
  awake = False
}

emptyWorld = World {
  gamelog = ["You should move.", "Welcome to The Sword"],
  worldMap = Map.empty,
  steps = 0,
  wMax = (0,0),
  hero = emptyHero,
  viewPort = ((10,0),(90,20)),
  monster = Map.empty
}

loadLevel :: String -> UTCTime -> World
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
	            worldMap = (Map.insert c Ground (worldMap wld))}
            'x' -> wld{monster = Map.insert c emptyMonster{
		    mlastMove = tnow} (monster wld),
		    worldMap = Map.insert c Ground (worldMap wld)}
            '#' -> wld{worldMap = Map.insert c Wall (worldMap wld)}
            '4' -> wld{worldMap = Map.insert c Tree (worldMap wld)}
            '.' -> wld{worldMap = Map.insert c Ground (worldMap wld)}
            otherwise -> error (show elt ++ " not recognized")

convertInput :: [Char] -> Input
convertInput (char:xs) =
  case char of
    'k' -> Up
    'j' -> Down
    'h' -> Left
    'l' -> Right
    'K' -> FightUp
    'J' -> FightDown
    'H' -> FightLeft
    'L' -> FightRight
    'q' -> Quit
    otherwise -> None
