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
    hSetNewlineMode handle universalNewlineMode
    hSetBuffering handle LineBuffering
    hPutStrLn handle "kmerz"
    worldMap <- hGetLine handle
    initGui
    _ <- race (fromServer (read worldMap :: WorldMap) emptyWorld) toServer
    return ()
  where
    fromServer worldMap world = do
      drawWorld worldMap world
      line <- hGetLine handle
      let newWorld = case line of
		   "" -> world
		   otherwise -> read line :: World
      fromServer worldMap newWorld
    toServer = do
      line <- getInput
      case line of
        "quit" -> do hPutStrLn handle "quit"; return "Quit"
        "" -> toServer
        _ ->  do hPutStrLn handle (line ++ "\n"); toServer
