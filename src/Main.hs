{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Tracking.Server
import Servant.API

import Control.Concurrent.STM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Time.Clock
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

type AdminAPI = "raw" :> QueryParam "num" Int :> Get '[JSON] [Map UTCTime String]

main :: IO ()
main = do
    logger <- memoryLogger 30000 31000
    return ()
