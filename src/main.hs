module Main where

import Network hiding (accept)
import Network.Socket
import System.Environment (getArgs)
import System.IO
import Prelude hiding (Either(..))
import Data.Char
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.Async (race)
import Control.Monad
import Control.Monad.Fix (fix)
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import qualified Data.Map as Map

import Sword.ViewPorter
import Sword.Utils
import Sword.World
import Sword.Hero
import Sword.Gui
import Sword.Daemon

main :: IO ()
main = do
  args <- getArgs
  case head args of
    "daemon" -> daemonStart
    "client" -> clientStart
    otherwise -> return ()

clientStart :: IO ()
clientStart = do
  handle <- connectTo "127.0.0.1" (PortNumber 4242)
  clientLoop handle `finally` finish handle

finish :: Handle -> IO ()
finish handle = do
    endGui
    hClose handle
    return ()

clientLoop :: Handle -> IO ()
clientLoop handle = do
    timeNow <- getCurrentTime
    hSetNewlineMode handle universalNewlineMode
    hSetBuffering handle LineBuffering
    hPutStrLn handle "kmerz"
    worldMap <- hGetLine handle
    me <- hGetLine handle
    initGui
    _ <- race (fromServer viewPort (read me :: Int) handle (read worldMap :: WorldMap) emptyWorld) (toServer handle timeNow)
    return ()
    where viewPort = ((0,0), (80,20))

fromServer :: ViewPort -> Int -> Handle -> WorldMap -> World -> IO ()
fromServer viewPort myself handle worldMap world = do
  let hero = Map.lookup myself (heros world)
  let viewPort' = updateViewPort viewPort hero (wMax worldMap)
  drawWorld hero viewPort' worldMap world
  line <- hGetLine handle
  let newWorld = case line of
           "" -> world
           otherwise -> read line :: World
  fromServer viewPort' myself handle worldMap newWorld

toServer :: Handle -> UTCTime -> IO ()
toServer handle lastMove = do
  timeNow <- getCurrentTime
  line <- getInput
  case (needToMove timeNow lastMove, line) of
    (_, "quit") -> do hPutStrLn handle "quit"; return ();
    (_, "") -> toServer handle lastMove
    (True, _) ->  do hPutStrLn handle (line ++ "\n"); toServer handle timeNow
    otherwise -> toServer handle lastMove
  where needToMove tNow last = diffUTCTime tNow last >= timeToNextMove
        timeToNextMove = 0.1
