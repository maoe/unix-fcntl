{-# LANGUAGE ViewPatterns #-}
import Control.Concurrent
import Control.Exception
import System.Environment

import Foreign.Var
import System.Posix.IO

import System.Posix.FileControl

main :: IO ()
main = do
  path:(read -> duration):_ <- getArgs
  flock <- newFlock
  flockType flock $= F_WRLCK
  flockWhence flock $= SEEK_SET
  flockStart flock $= 0
  flockLen flock $= 0
  bracket (openFd path ReadWrite Nothing defaultFileFlags) closeFd $ \fd -> do
    fcntl fd $ F_SETLK flock
    threadDelay $ duration * 10^6
