{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Tracking.Server where

import Prelude as P
import Data.Conduit
import Control.Monad.IO.Class (liftIO,MonadIO)
import Control.Monad.Trans.Resource
import qualified Data.Conduit.List as CL
import Data.Conduit.Network.UDP
import Data.Conduit.Binary
import Data.ByteString.Char8 as BS
--import Web.Scotty.Trans as S
--import GHC.Stats
import Network.Socket
import System.Exit
import System.Random
import Data.Time.Clock
--import Control.Monad
--import Control.Monad.Trans
--import Control.Concurrent.Async
--import Control.Concurrent.STM
--import qualified Data.Text.Lazy as T
--import qualified Data.Map.Strict as M


--runScotty :: Int -> ScottyT T.Text IO () -> IO ()
--runScotty p r = scottyT p id id r
--
--routes' :: TVar Store -> ScottyT T.Text IO ()
--routes' tvar = do
--    S.get "/" $ html "hello"
--    S.get "/bytes" $ do
--        store <- liftIO $ atomically $ readTVar tvar
--        let entries = M.size store
--
--        bytes <- lift $ fmap currentBytesUsed getGCStats
--        html $ T.pack $ unwords ["> "
--                                ,show bytes
--                                ,"bytes for"
--                                ,show entries
--                                ,"entries."
--                                ]
--
--openServer :: String -> HandlerFunc -> IO Server
--openServer port f = withSocketsDo $ do
--    addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
--                             Nothing
--                             (Just port)
--    case addrinfos of
--        (addr:_) -> runSocket addr f
--        _ -> do putStrLn $ "Could not find address info for" ++ port
--                exitFailure
--
--runSocket :: AddrInfo -> HandlerFunc -> IO Server
--runSocket addr f = do
--    sock <- socket (addrFamily addr) Datagram defaultProtocol
--    bindSocket sock $ addrAddress addr
--
--    a <- async $ forever $ do
--        (msg,_,addy) <- recvFrom sock 1024
--        f addy msg
--    return $ Server sock $ cancel a
--
--closeServer :: Server -> IO ()
--closeServer (Server sock io) = io >> close sock

getRecvInfo :: ServiceName -> IO (Socket, SockAddr)
getRecvInfo port = do
    addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                             Nothing
                             (Just port)
    addr <- case addrinfos of
                (addr:_) -> return addr
                _ -> do BS.putStrLn $ "Could not find address info for " `BS.append` (pack $ show port)
                        exitFailure

    sock <- socket (addrFamily addr) Datagram defaultProtocol
    return (sock, addrAddress addr)

udpSource :: MonadResource m => ServiceName -> Producer m ByteString
udpSource port = bracketP create destroy run
    where create = do s <- getRecvInfo port
                      uncurry bindSocket s
                      return s
          destroy = sClose . fst
          run s = sourceSocket (fst s) 1024 =$ (CL.map clearMsg) =$ (CL.map (`BS.append` "\n"))
              where clearMsg (Message msg _) = BS.map (\c -> if c == '\n'
                                                             then ' '
                                                             else c)
                                                      msg

udpToFile :: ServiceName -> FilePath -> IO ()
udpToFile port file = runResourceT $ (udpSource port $$ sinkFile file)

getMsg :: Source IO ByteString
getMsg = do
    str <- liftIO BS.getLine
    yield $ str

getSendInfo :: HostName -> ServiceName -> IO (Socket, SockAddr)
getSendInfo host port = do
    addrinfos <- getAddrInfo Nothing (Just host) (Just port)
    case addrinfos of
        (addr:_) -> do sock <- socket (addrFamily addr) Datagram defaultProtocol
                       return $ (sock, addrAddress addr)
        _        -> do BS.putStrLn $ "Could not find address info for 31000"
                       exitFailure

--sendMsg :: HostName -> ServiceName -> Consumer ByteString IO ()
sendMsg host port = do
    (sock,addr) <- liftIO $ getSendInfo host port
    CL.map (\bs -> Message bs addr) $= sinkToSocket sock

client :: IO ()
client = getMsg $$ sendMsg "127.0.0.1" "31000"
    --(sock, addr) <- getSendInfo "127.0.0.1" "31000"
    --forever $ do ln <- P.getLine
    --             send' ln sock addr
    --where send' [] _ _ = return ()
    --      send' msg s a = do sent <- sendTo s msg a
    --                         send' (P.drop sent msg) s a

testClient :: HostName -> ServiceName -> NominalDiffTime -> IO ()
testClient host port seconds = do
    t           <- getCurrentTime
    (sock,addr) <- getSendInfo host port
    g           <- newStdGen
    loop' t (randomRs (0,1000) g) sock addr 0
    where loop' :: UTCTime -> [Int] -> Socket -> SockAddr -> Int -> IO ()
          loop' t (r:rs) s a n = do send' (show r) s a
                                    t' <- getCurrentTime
                                    if addUTCTime seconds t <= t'
                                    then P.putStrLn $ "Sent " ++ (show n) ++ " messages."
                                    else loop' t rs s a (n + 1)
          loop' _ _      _ _ _ = return ()
          send' []  _ _ = return ()
          send' msg s a = do sent <- sendTo s msg a
                             send' (P.drop sent msg) s a
