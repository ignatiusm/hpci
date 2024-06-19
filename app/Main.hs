module Main (main) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8

import Data.Char (ord)
import Control.Concurrent
import System.Environment
import System.FilePath

import Network.SSH.Client.LibSSH2.Foreign
import Network.SSH.Client.LibSSH2

runCommand :: Session -> String -> IO (Int, BSL.ByteString)
runCommand s cmd = withChannel s $ \ch -> do
         channelExecute ch cmd
         readAllChannel ch

parseSubmissionResult :: (Int, BSL.ByteString) -> String
parseSubmissionResult = show . head . BSL.split (fromIntegral (ord '.')) . snd

parseQstatResponse :: (Int, BSL.ByteString) -> String
parseQstatResponse r = head $ tail $ reverse $ words $ head $ drop 2 $ lines $ BSL8.unpack $ snd r

-- make execute command function, then can

checkStatus :: Session -> String -> IO ()
checkStatus s jid = do
  jobStatus <- runCommand s ("qstat " ++ jid)
  let status = parseQstatResponse jobStatus
  putStrLn status

-- pollUntilFinished :: Session -> String -> IO ()
-- pollUntilFinished s jid
--   | r == "F"  = return ()
--   | otherwise = pollUntilFinished session jid
--   where
--     do
--       r <- checkStatus s jid

-- TODO: add sshCred datatype (will make type signature less overwhelming), can have constructor to adds some defaults
-- data Entry = Entry { langName :: String, perf3 :: Int, totalChars :: Int} deriving Show
-- TODO: add parser for a config file; and for parsing job id; parse status of job
-- TODO: explore Reader monad to replace global variables and thread config through code.

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

  -- Submit job using script file
  submissionResult <- runCommand session (command ++ " " ++ script)

  let jobId = parseSubmissionResult submissionResult

  putStrLn ("Job ID: " ++ jobId)

  -- Query job status
  -- need data type for status
  -- jobStatus <- runCommand session ("qstat " ++ jobId)

  -- let status = parseQstatResponse jobStatus

  -- putStrLn ("Job Status: " ++ status) 
  checkStatus session jobId

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
