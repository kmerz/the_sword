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

{--gameLoop :: World -> IO ()
gameLoop world = do
  drawWorld world
  tnow <- getCurrentTime
  input <- getInput
  if input == Quit  || (life (hero (world))) <= 0
    then return ()
    else gameLoop $ modifyWorld input tnow world --}

main :: IO ()
main = do
  args <- getArgs
  case (head args) of
    "deamon" -> daemonStart
    "client" -> clientStart
    otherwise -> return ()

clientStart :: IO ()
clientStart = do
  handle <- connectTo "127.0.0.1" (PortNumber 4242)
  clientLoop handle `finally` (finish handle)

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
    hPutStrLn handle "k"
    initGui
    _ <- race (fromServer emptyWorld) toServer
    return ()
  where
    fromServer world = do
      drawWorld (world)
      line <- hGetLine handle
      newWorld <- return $ case line of
		   "" -> world
		   otherwise -> (read line) :: World
      fromServer newWorld
    toServer = do
      line <- getInput
      case line of
        "quit" -> do hPutStrLn handle "quit"; return "Quit"
        "" -> do toServer
        _ ->  do hPutStrLn handle (line ++ "\n"); toServer
