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

type Msg = (Int, String, String)

daemonStart :: IO ()
daemonStart = do
  timeNow <- getCurrentTime
  level <- readFile "src/levels/0A_level.txt"
  let (world, worldMap) = loadLevel level timeNow
  chan <- newChan
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bindSocket sock (SockAddrInet 4242 iNADDR_ANY)
  -- allow a maximum of 2 outstanding connections
  listen sock 2
  forkIO (daemonGameLoop chan world worldMap)
  daemonAcceptLoop worldMap sock chan 1

daemonGameLoop :: Chan Msg -> World -> WorldMap -> IO ()
daemonGameLoop chan world worldM = do
  (nr, input, arg) <- readChan chan
  tnow <- getCurrentTime
  case (nr, input, arg) of
    (0, _, _) ->
      daemonGameLoop chan (modifyWorld 0 None tnow worldM world) worldM
    (x, "", _) ->
      daemonGameLoop chan (modifyWorld 0 None tnow worldM world) worldM
    (x, input, "") -> do
      let newWorld = modifyWorld x (convertInput input) tnow worldM world
      writeChan chan (0, show newWorld ++ "\n", "")
      daemonGameLoop chan newWorld worldM
    (x, "login", name) ->
      daemonGameLoop chan (addHero name x tnow world) worldM
    otherwise -> do
      --putStrLn "??: " ++ nr ++ " " ++ input ++ " " ++ arg
      daemonGameLoop chan world worldM

daemonAcceptLoop :: WorldMap -> Socket -> Chan Msg -> Int -> IO ()
daemonAcceptLoop wldMap sock chan nr = do
  conn <- accept sock
  forkIO (runConn conn chan nr wldMap)
  daemonAcceptLoop wldMap sock chan $! nr + 1

runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> WorldMap -> IO ()
runConn (sock, _) chan nr worldMap = do
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl LineBuffering
  name <- liftM init (hGetLine hdl)
  hPutStrLn hdl (show worldMap ++ "\n")
  chan' <- dupChan chan
  writeChan chan' (nr, "login", name)
  reader <- forkIO $ fix $ \loop -> do
    (nr', line, _) <- readChan chan'
    when (nr' == 0) $ hPutStrLn hdl line
    hFlush hdl
    loop
  handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
    line <- hGetLine hdl
    case line of
      "quit" -> hPutStrLn hdl "Bye!"
      _ -> do
        writeChan chan (nr, line, "")
	loop
    killThread reader
    hClose hdl
    loop

emptyMonster = Monster {
  mlife = 5,
  awake = False
}

emptyWorld = World {
  gamelog = ["You should move.", "Welcome to The Sword"],
  heros = Map.empty,
  viewPort = ((10,0),(90,20)),
  monster = Map.empty
}

loadLevel :: String -> UTCTime -> (World, WorldMap)
loadLevel str tnow = foldl consume (emptyWorld, Map.empty) elems
  where lns     = lines str
        coords  = [[(x,y) | x <- [0..]] | y <- [0..]]
        elems   = concat $ zipWith zip coords lns
        consume (wld, wldMap) (c, elt) =
          case elt of
	    '@' -> (wld, Map.insert c Ground wldMap)
            'x' -> (wld{monster = Map.insert c emptyMonster{mlastMove = tnow} (monster wld)},
		     Map.insert c Ground wldMap)
            '#' -> (wld, Map.insert c Wall wldMap)
            '4' -> (wld, Map.insert c Tree wldMap)
            '.' -> (wld, Map.insert c Ground wldMap)
            otherwise -> error (show elt ++ " not recognized")

convertInput :: String -> Input
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
