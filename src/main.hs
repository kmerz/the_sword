module Main where

import System.Environment (getArgs)

import Sword.Daemon
import Sword.Client

main :: IO ()
main = do
  args <- getArgs
  case head args of
    "daemon" -> daemonStart
    "client" -> clientStart (tail args)
    otherwise -> return ()
