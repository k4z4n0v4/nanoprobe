{-# LANGUAGE DeriveGeneric #-}

module Scanner (
    Target (..),
    scan,
) where

import Data.Text (Text)
import GHC.Generics
import Control.Concurrent (threadDelay)

data Target = Target
    { project :: Text
    , host :: Text
    }
    deriving (Show, Eq, Generic)

-- Stub, will be replaced with actual scanning logic
scan :: Target -> IO ()
scan target = do
  threadDelay 10000000
  print target  
