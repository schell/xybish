{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Tracking.Server

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import qualified Data.Map.Strict as M

runLogger :: MemoryLogger -> IO ()
runLogger = run M.empty
    where run m logger = do m' <- readTVarIO $ loggerUser logger
                            if m' == m
                            then do threadDelay 10000
                                    run m' logger
                            else do print $ M.difference m' m
                                    run m' logger

main :: IO ()
main = do
    logger <- memoryLogger 30000 31000
    a <- async $ runLogger logger
    _ <- getLine
    cancel a
    closeLogger logger
