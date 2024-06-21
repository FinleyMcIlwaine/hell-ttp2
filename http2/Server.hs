module Server where

import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as C8
import Data.String
import System.IO
import qualified UnliftIO.Exception as E

-- http-types
import Network.HTTP.Types

-- network-run
import Network.Run.TCP (runTCPServer) -- network-run

-- http2
import Network.HTTP2.Client as Client
import Network.HTTP2.Server as Server

runServer :: Word -> IO ()
runServer bodyBytes =
    runTCPServer (Just "localhost") "12080" runHTTP2Server
  where
    runHTTP2Server s =
        allocSimpleConfig s 4096 `E.bracket` freeSimpleConfig $
          \conf ->
            Server.run
              defaultServerConfig
              conf
              server

    server _req _aux sendResponse = sendResponse response []
      where
        response = responseBuilder ok200 header body
        header   = [(fromString "Content-Type", C8.pack "text/plain")]
        body     = BSB.string8 $ take (fromIntegral bodyBytes) $ cycle ['0' .. '9']
