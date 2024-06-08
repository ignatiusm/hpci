module Main (main) where

import Network.SSH.Client.SimpleSSH
import Control.Monad.Trans.Except (runErrorT)

main :: IO ()
main = do
  let hostname = "gadi.nci.org.au"
      port = 22
      username = "im9095"
      publicKey = "~/.ssh/id_rsa.pub"

  result <- runErrorT $ do 
    session <- openSession hostname port
    authenticateWithPublicKeyFile session username publicKey
    runCommand session "ls -l"

  case result of
    Left err -> putStrLn $ "SSH Error: " ++ show err
    Right output -> putStrLn output
