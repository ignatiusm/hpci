module Main (main) where

import qualified Data.ByteString.Lazy as BSL
import System.Environment
import System.FilePath

import Network.SSH.Client.LibSSH2.Foreign
import Network.SSH.Client.LibSSH2

main :: IO()
main = do
  args <- getArgs
  case args of 
    [user, host, port, cmd, knownHost, public, private, script] -> runCommand user host (read port) cmd knownHost public private script
    _ -> putStrLn "run with following args: username hostname port cmd knownHosts publickey privatekey script"

ssh :: String -> String -> Int -> FilePath -> FilePath -> FilePath -> (Session -> IO a) -> IO()
ssh login host port knownHost public private actions = do
  initialize True
  withSSH2 knownHost public private "" login host port $ actions
  exit

runCommand :: String -> String -> Int -> String -> FilePath -> FilePath -> FilePath -> FilePath -> IO()
runCommand user host port command knownHost public private script =
  ssh user host port knownHost public private $ \s -> do
    sz <- scpSendFile s 0o644 script (takeFileName script)
    putStrLn $ "Sent: " ++ show sz ++ " bytes."
    withChannel s $ \ch -> do
      channelExecute ch command
      result <- readAllChannel ch
      BSL.putStr result
