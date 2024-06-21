module Main where

import Client
import Server

import Control.Concurrent
import System.Environment

defaultServerBodyBytes :: Word
defaultServerBodyBytes = 5000

defaultClientRequests :: Word
defaultClientRequests = 400

main :: IO ()
main = do
    (arg : args) <- getArgs
    case arg of
      "client" ->
        runClient $
          case args of
            n : _ -> read n
            []    -> defaultClientRequests
      "server" ->
        runServer $
          case args of
            n : _ -> read n
            []    -> defaultServerBodyBytes
      "both" ->
        case args of
          n : m : _ -> do
            _serverThreadId <- forkIO $ runServer (read m)
            threadDelay 10000
            runClient (read n)
          _ -> do
            _serverThreadId <- forkIO $ runServer defaultServerBodyBytes
            threadDelay 10000
            runClient defaultClientRequests
      _ -> error "specify `client`, `server`, or `both`"
