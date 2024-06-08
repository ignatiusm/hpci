module Main (main) where

import Network.SSH.Client.SimpleSSH
import Control.Monad.Except

main :: IO ()
main = do
  let hostname = "hostname"
      port = 22
      username = "abc123"
      publicKey = "~/.ssh/id_rsa.pub"
      privateKey = "~/.ssh/id_rsa"
      knownHosts = "~/.ssh/known_hosts"

  result <- runExceptT $ do
    session <- openSession hostname port knownHosts
    _ <- authenticateWithKey session username publicKey privateKey ""
    execCommand session "ls -l"

  case result of
    Left err -> putStrLn $ "SSH Error: " ++ show err
    Right output -> putStrLn $ show output
