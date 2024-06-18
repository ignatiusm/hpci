module Main (main) where

import qualified Data.ByteString.Lazy as BSL
import System.Environment
import System.FilePath

import Network.SSH.Client.LibSSH2.Foreign
import Network.SSH.Client.LibSSH2

runHpci :: String -> String -> Int -> String -> FilePath -> FilePath -> FilePath -> FilePath -> IO ()
runHpci user host port command knownHost public private script = do
  --  Initialize session
  session <- sessionInit host port
  putStrLn "Start Session"

  -- Check remote host against known hosts list
  _ <- checkHost session host port knownHost [TYPE_MASK]

  -- Authenticate
  publicKeyAuthFile session user public private ""
  putStrLn "Authorised"

  -- Send a file to remote host via SCP.
  sz <- scpSendFile session 0o644 script (takeFileName script)
  putStrLn $ "Sent: " ++ show sz ++ " bytes."

  -- Execute some actions within SSH2 channel
  _ <- withChannel session $ \ch -> do
         channelExecute ch (command ++ " " ++ script)
         result <- readAllChannel ch
         BSL.putStr result

  -- TODO: poll status of job, until error or finished (with timeout?)

  -- TODO: copy logs file off server to ci

  -- Close active session
  sessionClose session
  putStrLn "Closed Session"

  -- TODO: print logs file if there is one


main :: IO()
main = do
  args <- getArgs
  case args of 
    [user, host, port, cmd, knownHost, public, private, script] -> runHpci user host (read port) cmd knownHost public private script
    _ -> putStrLn "required args: username hostname port cmd knownHosts publickey privatekey script"
