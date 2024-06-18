module Main (main) where

import qualified Data.ByteString.Lazy as BSL
import System.Environment
import System.FilePath
import Control.Concurrent

import Network.SSH.Client.LibSSH2.Foreign
import Network.SSH.Client.LibSSH2

runHpci :: String -> String -> Int -> String -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> IO ()
runHpci user host port command knownHost public private script logFile = do
  --  Initialize session
  session <- sessionInit host port
  putStrLn "Start Session"

  -- Check remote host against known hosts list
  _ <- checkHost session host port knownHost [TYPE_MASK]

  -- Authenticate
  publicKeyAuthFile session user public private ""
  putStrLn "Authorised"

  -- Send a file to remote host via SCP.
  scriptSize <- scpSendFile session 0o644 script (takeFileName script)
  putStrLn $ "Sent: " ++ script ++ " - "++ show scriptSize ++ " bytes."

  -- Execute some actions within SSH2 channel
  _ <- withChannel session $ \ch -> do
         channelExecute ch (command ++ " " ++ script)
         result <- readAllChannel ch
         BSL.putStr result

  -- TODO: poll status of job, until error or finished (with timeout?)
  putStrLn "15 second delay to allow for job to finish before attempting to copy log off server"
  threadDelay 15000000
  
  -- Copy logs file off server to ci
  logSize <- scpReceiveFile session logFile logFile
  putStrLn $ "Received: " ++ logFile ++ " - " ++ show logSize ++ " bytes."

  -- Remove script from server
  _ <- withChannel session $ \ch -> do
         channelExecute ch ("rm " ++ script)
         result <- readAllChannel ch
         BSL.putStr result

  -- Close active session
  sessionClose session
  putStrLn "Closed Session"

  -- Print logs file
  contents <- readFile logFile
  putStrLn "Contents of log file:"
  putStr contents


main :: IO()
main = do
  args <- getArgs
  case args of 
    [user, host, port, cmd, knownHost, public, private, script, logFile] -> runHpci user host (read port) cmd knownHost public private script logFile
    _ -> putStrLn "required args: username hostname port cmd knownHosts publickey privatekey script logFile"
