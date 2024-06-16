module Main (main) where

import qualified Data.ByteString.Lazy as BSL

import Network.SSH.Client.LibSSH2.Foreign
import Network.SSH.Client.LibSSH2
import System.Environment
import System.FilePath

main = do
  args <- getArgs
  case args of 
    [user, host, port, cmd, knownHost, public, private] -> runCommand user host (read port) cmd knownHost public private
    _ -> putStrLn "run with following args: username hostname port cmd knownHosts publickey privatekey"

ssh login host port knownHost public private actions = do
  initialize True
  withSSH2 knownHost public private "" login host port $ actions
  exit

runCommand user host port command knownHost public private =
  ssh user host port knownHost public private $ \s ->
    withChannel s $ \ch -> do
      channelExecute ch command
      result <- readAllChannel ch
      BSL.putStr result
