{-# LANGUAGE OverloadedStrings #-}
module Client where

import Control.Monad
import Data.ByteString qualified as BS
import System.IO
import UnliftIO.Exception

-- network-run
import Network.HTTP.Types
import Network.Run.TCP (runTCPClient)

-- http2
import Network.HTTP2.Client as Client



runClient :: Word -> IO ()
runClient numRequests =
    runTCPClient "localhost" "12080" runHTTP2Client
  where
    runHTTP2Client s =
        allocSimpleConfig s 4096 `bracket` freeSimpleConfig $
          \conf ->
            Client.run
              (defaultClientConfig { authority = "localhost" })
              conf
              client

    client :: Client ()
    client sendRequest _aux =
        forM_ [0 .. numRequests :: Word] $ \i -> do
          when (i `mod` 50 == 0) $ print i
          let req = requestNoBody methodGet "/" []
          sendRequest req $ \rsp -> do
            -- hPutStrLn stderr "response received"
            let _readAll r = do
                  c <- getResponseBodyChunk r
                  if BS.null c then
                    return ()
                  else
                    _readAll r
            !r <- _readAll rsp
            -- hPutStrLn stderr $ "response chunk (len " ++ show (BS.length r) ++ "): " ++ show r
            return ()