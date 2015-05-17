{-# LANGUAGE OverloadedStrings #-}
module Main where

import Tracking.Types
import Tracking.Server
import Tracking.Parse

import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.Time.Clock
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M

{-
6077 entries is 37107104 bytes
0 entries is 27591632 bytes
-}

xybish :: String -> IO Server
xybish port = do
    tvar <- atomically $ newTVar M.empty
    (Server sock io) <- openServer port $ \_ msg -> do
        case parseOnly parseInsertion $ B.pack msg of
            Left _ -> return ()
            Right (k,v) -> do t <- getCurrentTime
                              atomically $ modifyTVar' tvar $ M.insert (t,k) v
    a' <- async $ runScotty 8000 $ routes' tvar
    return $ Server sock $ cancel a' >> io



main :: IO ()
main = putStrLn "xybish"

