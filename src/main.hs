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
import Control.Monad
import Control.Monad.Fix (fix)
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import qualified Data.Map as Map

import Sword.Utils
import Sword.World
import Sword.Hero
import Sword.Gui
import Sword.Daemon

gameLoop :: World -> IO ()
gameLoop world = do
  drawWorld world
  tnow <- getCurrentTime
  input <- getInput
  if input == Quit  || (life (hero (world))) <= 0
    then return ()
    else gameLoop $ modifyWorld input tnow world

main :: IO ()
main = do
  args <- getArgs
  if (head args) == "deamon"
    then daemonStart
    else clientStart

clientStart :: IO ()
clientStart = do
  initGui
  --gameLoop world
  endGui
