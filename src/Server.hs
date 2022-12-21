{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Server (
    srv,
    initialScanState,
) where

import Control.Concurrent (forkFinally)
import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Monad.Logger (MonadLoggerIO)
import Data.Aeson (Value (Null))
import Data.Aeson.TH (deriveFromJSON, deriveToJSON)
import Data.Aeson.Types (
    FromJSON (parseJSON),
    defaultOptions,
 )
import Data.HashMap.Lazy as Map (HashMap, empty, insert, (!?))
import Data.Hashable (Hashable)
import Data.IORef (modifyIORef, readIORef)
import Data.Text (Text, unpack)
import GHC.IORef (IORef)
import Network.JSONRPC (
    ErrorObj (ErrorObj),
    FromRequest (..),
    JSONRPCT,
    buildResponse,
    receiveRequest,
    sendResponse,
 )

import Scanner

instance Hashable Target

data Status = Running | Failed | Success deriving (Show)

type StateMap = Map.HashMap Target Status

initialScanState :: StateMap
initialScanState = empty

-- All hail Template Haskell
$(deriveFromJSON defaultOptions ''Target)
$(deriveToJSON defaultOptions ''Status)

{- | For parsing incoming jsonrpc
 Requests should be in form of:
 {"jsonrpc": "2.0", "method": "scan", "params": {"project": "django", "host": "10.10.10.10"}, "id": 1}
-}
instance FromRequest Target where
    parseParams "scan" = Just parseJSON
    parseParams _ = Nothing

-- | Get target and start scan or report status.
generateResponse :: (MonadLoggerIO m) => IORef StateMap -> Target -> m (Either ErrorObj Text)
generateResponse state target = do
    scanState <- liftIO $ readIORef state
    case scanState !? target of
        Nothing -> do
            liftIO $ scanAndWatch state target
            return . Right $ "running " <> message
        Just Running -> return . Right $ message <> " is already running."
        Just Success -> return . Right $ message <> " ended successfully."
        Just Failed -> return $ Left genErrorObj
  where
    genErrorObj :: ErrorObj
    genErrorObj = ErrorObj (unpack message <> " failed.") (-32000) Null

    message :: Text
    message = mconcat ["scan on ", host target, " with configuration ", project target]


-- Start scan on target and update status accordingly
scanAndWatch :: IORef StateMap -> Target -> IO ()
scanAndWatch state target = do
    void $ forkFinally (scan target) handleException
    changeState state target Running
  where
    handleException e = case e of
        Left _ -> changeState state target Failed
        Right _ -> changeState state target Success

-- Helper function for setting target's Status in a state
changeState :: IORef StateMap -> Target -> Status -> IO ()
changeState state target status = modifyIORef state $ Map.insert target status


-- Main server
srv :: (MonadLoggerIO m) => IORef StateMap -> JSONRPCT m ()
srv state = do
    maybeRequest <- receiveRequest
    case maybeRequest of
        Just request -> do
            mResponse <- buildResponse (generateResponse state) request
            mapM_ sendResponse mResponse
            srv state
        Nothing -> return ()
