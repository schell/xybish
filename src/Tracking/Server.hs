{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Tracking.Server where

import Tracking.Database
import Prelude as P
import Data.Conduit
import Control.Monad.Trans.Resource
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Logger
import qualified Data.Conduit.List as CL
import Data.Conduit.Network.UDP as UDP
import Data.Conduit.Binary as CB
import Data.Conduit.TMChan
import Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Data.Time.Clock
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Database.Persist
import Database.Persist.Sqlite
import Network.Socket
import Network.Wai
import Network.Wai.Handler.Warp as W hiding (cancel)
import Network.HTTP.Types
import Network.HTTP.Conduit
import System.Exit
import System.Random

--------------------------------------------------------------------------------
-- Logger
--------------------------------------------------------------------------------
data Logger a = Logger { loggerChan :: TBMChan ByteString
                       , loggerUDP  :: Async ()
                       , loggerHTTP :: Async ()
                       , loggerUser :: a
                       }

openLogger :: Int -> Int -> IO (Logger ())
openLogger udpPort httpPort = do
    chan <- atomically (newTBMChan 1024)

    Logger chan <$> async (udpLogger chan udpPort)
                <*> async (httpLogger chan httpPort)
                <*> pure ()

closeLogger :: Logger a -> IO ()
closeLogger (Logger chan a b _) = do
    atomically $ closeTBMChan chan
    P.putStrLn "Killing udp and http logger..."
    cancel a
    cancel b
    P.putStrLn "Done."
--------------------------------------------------------------------------------
-- Specialized loggers
--------------------------------------------------------------------------------
fileLogger :: Int -> Int -> IO (Logger FilePath)
fileLogger udpPort httpPort = do
    logger <- openLogger udpPort httpPort

    let file = "db.txt"
        chan = loggerChan logger

    void $ async $ runResourceT $ sourceTBMChan chan $$ (mkMsg $= toTupleBS)
                                                     =$ sinkFile file
    return logger{ loggerUser = file }


type MemoryLogger = Logger (TVar (Map UTCTime ByteString))

memoryLogger :: Int -> Int -> IO MemoryLogger
memoryLogger udpPort httpPort = do
    tvar   <- atomically $ newTVar M.empty
    logger <- openLogger udpPort httpPort

    let chan = loggerChan logger

    void $ async $ runResourceT $ sourceTBMChan chan $$ mkMsg =$
        CL.mapM_ (\(Entry t msg) ->
            liftIO $ atomically $ modifyTVar' tvar $ M.insert t msg)
    return logger{ loggerUser = tvar }

sqliteLogger :: Int -> Int -> IO (Logger FilePath)
sqliteLogger udpPort httpPort = do
    logger <- openLogger udpPort httpPort

    let connections = 10
        file = "db.db3"
        chan = loggerChan logger

    void $ async $ runNoLoggingT $ withSqlitePool file connections $ \pool ->
        liftIO $ runResourceT $ flip runSqlPool pool $ do
            runMigration migrateAll
            sourceTBMChan chan $$ mkMsg =$ CL.mapM_ (void . insert)

    return logger{ loggerUser = T.unpack file }
--------------------------------------------------------------------------------
-- HTTP
--------------------------------------------------------------------------------
httpLogger :: TBMChan ByteString -> Int -> IO ()
httpLogger chan prt = runSettings set $ httpApp chan
    where set = W.setPort prt defaultSettings

httpApp :: TBMChan ByteString -> Application
httpApp chan req respond = do
    let bs = BS.drop 1 $ rawPathInfo req
    runResourceT $ void (yield bs) $$ sinkTBMChan chan False
    respond $ responseLBS status200 [] "ok"
--------------------------------------------------------------------------------
-- UDP
--------------------------------------------------------------------------------
udpLogger :: TBMChan ByteString -> Int -> IO ()
udpLogger chan prt = runResourceT $
    udpSource (show prt) $$ sinkTBMChan chan False

udpSource :: MonadResource m => ServiceName -> Producer m ByteString
udpSource prt = bracketP create destroy go
    where create = do P.putStrLn "Creating udp source."
                      s <- getRecvInfo prt
                      uncurry bindSocket s
                      return s
          destroy (s,_) = do sClose s
                             P.putStrLn "Destroyed udp source."
          go s = UDP.sourceSocket (fst s) 1024 =$ CL.map msg
              where msg (Message m _) = m

getRecvInfo :: ServiceName -> IO (Socket, SockAddr)
getRecvInfo prt = do
    addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                             Nothing
                             (Just prt)
    addr <- case addrinfos of
                (addr:_) -> return addr
                _ -> do BS.putStrLn $ BS.append
                                        "Could not find address info for "
                                        (BS.pack $ show prt)
                        exitFailure

    sock <- socket (addrFamily addr) Datagram defaultProtocol
    return (sock, addrAddress addr)

getSendInfo :: HostName -> ServiceName -> IO (Socket, SockAddr)
getSendInfo hst prt = do
    addrinfos <- getAddrInfo Nothing (Just hst) (Just prt)
    case addrinfos of
        (addr:_) -> do sock <- socket (addrFamily addr) Datagram defaultProtocol
                       return (sock, addrAddress addr)
        _        -> do BS.putStrLn $ BS.unwords [ "Could not find address info for"
                                                , BS.pack hst
                                                , BS.pack prt
                                                ]
                       exitFailure
--------------------------------------------------------------------------------
-- Testing
--------------------------------------------------------------------------------
testUdpClient :: HostName -> ServiceName -> Int -> IO ()
testUdpClient hst prt n = do
    t           <- getCurrentTime
    (sock,addr) <- getSendInfo hst prt
    g           <- newStdGen
    loop' t (randomRs (' ','~') g)
            (randomRs (1,1024) g :: [Int])
            sock
            addr
            (0 :: Int)
    where loop' t str is s a n'
            | n' >= n = do
                t' <- getCurrentTime
                let ss = diffUTCTime t' t
                P.putStrLn $ "Completed " ++ show n' ++ " requests in " ++ show ss ++ " seconds."

            | otherwise = do
                send' (P.take (P.head is) str) s a
                loop' t (P.drop 100 str) (P.drop 1 is) s a (n' + 1)
          send' []  _ _ = return ()
          send' msg s a = do sent <- sendTo s msg a
                             send' (P.drop sent msg) s a

testHttpClient :: String -> Int -> IO ()
testHttpClient url n = do
    g <- newStdGen
    t <- getCurrentTime
    loop' t 0 (randomRs ('a','z') g)
              (randomRs (1,1024) g :: [Int])
    where loop' t n' str is
            | n' >= n   = do t' <- getCurrentTime
                             let s = diffUTCTime t' t
                             P.putStrLn $ "Completed " ++ show n' ++ " requests in " ++ show s ++ " seconds."

            | otherwise = do let url' = url ++ "/" ++ s
                                 s    = P.take (P.head is) str
                                 str' = P.drop (P.length s) str
                                 is'  = P.drop 1 is
                             void $ simpleHttp url'
                             loop' t (n'+1) str' is'

linesOfFile :: MonadResource m => FilePath -> Producer m ByteString
linesOfFile file = sourceFile file $= CB.lines

runFoldFile :: FilePath -> (b -> ByteString -> b) -> b -> IO b
runFoldFile file f acc = runResourceT $ linesOfFile file $$ CL.fold f acc

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
pollChanStatus :: forall a. TBMChan a -> IO ()
pollChanStatus chan = void $ async $ forever $ do
    closed <- atomically $ isClosedTBMChan chan
    slots  <- atomically $ freeSlotsTBMChan chan
    print $ "Channel is " ++
            (if closed then "closed" else "open") ++
            " with " ++ show slots ++ " slots available."
    threadDelay 5000000

-- Creates an Entry given an incoming message.
mkMsg :: MonadIO m => Conduit ByteString m Entry
mkMsg = CL.mapM $ \msg -> do
    t <- liftIO getCurrentTime
    return $ Entry t msg

toTupleBS :: Monad m => Conduit Entry m ByteString
toTupleBS = CL.map $ \(Entry t msg) -> BS.pack (show (t,msg)) `BS.append` "\n"
