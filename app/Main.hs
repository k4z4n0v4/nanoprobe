{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.Logger
import Control.Monad.State (liftIO)
import Data.Conduit.Network
import Data.IORef (newIORef)
import Network.JSONRPC
import Server (initialScanState, srv)

settings :: ServerSettings
settings = serverSettings 31337 "127.0.0.1"

logFilter :: LogSource -> LogLevel -> Bool
logFilter _ LevelError = True
logFilter _ LevelWarn = True
logFilter _ LevelInfo = True
logFilter _ LevelDebug = False
logFilter _ (LevelOther _) = False

main :: IO ()
main =
    runStdoutLoggingT $
        filterLogger logFilter $ do
            state <- liftIO $ newIORef initialScanState
            jsonrpcTCPServer V2 False settings (srv state)
