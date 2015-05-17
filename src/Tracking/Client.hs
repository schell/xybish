{-# LANGUAGE OverloadedStrings #-}
module Tracking.Client where

import Web.Scotty.Trans as S
import Web.Scotty.Internal.Types
import Web.Scotty (ScottyM)
import GHC.Stats
import Network.Socket
import System.Exit
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.Async
import Control.Concurrent
import Control.Concurrent.STM
import Data.Scientific (Scientific)
import Data.Map.Strict (Map)
import Data.Time.Clock
import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import qualified Data.Text.Lazy as T
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M

data Client = Client Socket SockAddr deriving (Show, Eq)

openClient :: String -> String -> IO Client
openClient host port = do
    addrinfos <- getAddrInfo Nothing (Just host) (Just port)
    case addrinfos of
        (addr:_) -> do sock <- socket (addrFamily addr) Datagram defaultProtocol
                       return $ Client sock $ addrAddress addr

        _        -> do putStrLn $ unwords ["Could not find address info for"
                                          ,host,port
                                          ]
                       exitFailure

sendClient :: Client -> String -> IO ()
sendClient _ [] = return ()
sendClient c@(Client sock address) msg = do
    sent <- sendTo sock msg address
    sendClient c $ drop sent msg

closeClient :: Client -> IO ()
closeClient (Client s _) = sClose s
