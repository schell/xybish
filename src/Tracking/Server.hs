{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Tracking.Server where

import Prelude as P
import Data.Conduit
import Control.Monad.Trans.Resource
import Control.Concurrent.Async
import Control.Monad.STM
import qualified Data.Conduit.List as CL
import Data.Conduit.Network.UDP as UDP
import Data.Conduit.Binary as CB
import Data.Conduit.TMChan
import Data.ByteString.Char8 as BS
import Network.Socket
import Network.Wai
import Network.Wai.Handler.Warp as W
import Network.HTTP.Types
import System.Exit
import System.Random
import Data.Time.Clock

--------------------------------------------------------------------------------
-- Logger
--------------------------------------------------------------------------------

logger :: Int -> Int -> FilePath -> IO ()
logger udpPort httpPort file = do
    chan <- atomically $ newTBMChan 1024
    _ <- async $ udpLogger chan udpPort
    _ <- async $ httpLogger chan httpPort
    runResourceT $ sourceTBMChan chan $$ sinkFile file

--------------------------------------------------------------------------------
-- TCP
--------------------------------------------------------------------------------

httpLogger :: TBMChan ByteString -> Int -> IO ()
httpLogger chan port = runSettings set $ httpApp chan
    where set = W.setPort port defaultSettings

httpApp :: TBMChan ByteString -> Application
httpApp chan req respond = do
    let bs = clearMsg $ BS.drop 1 $ rawPathInfo req
    runResourceT $ (yield bs >> return ()) $$ sinkTBMChan chan False
    respond $ responseLBS status200 [] "ok"

--------------------------------------------------------------------------------
-- UDP
--------------------------------------------------------------------------------

udpLogger :: TBMChan ByteString -> Int -> IO ()
udpLogger chan port = runResourceT $
    udpSource (show port) $$ sinkTBMChan chan False

udpSource :: MonadResource m => ServiceName -> Producer m ByteString
udpSource port = bracketP create destroy go
    where create = do s <- getRecvInfo port
                      uncurry bindSocket s
                      return s
          destroy = sClose . fst
          go s = UDP.sourceSocket (fst s) 1024 =$ (CL.map msg)
              where msg (Message m _) = clearMsg m

getRecvInfo :: ServiceName -> IO (Socket, SockAddr)
getRecvInfo port = do
    addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                             Nothing
                             (Just port)
    addr <- case addrinfos of
                (addr:_) -> return addr
                _ -> do BS.putStrLn $ BS.append
                                        "Could not find address info for "
                                        (pack $ show port)
                        exitFailure

    sock <- socket (addrFamily addr) Datagram defaultProtocol
    return (sock, addrAddress addr)

getSendInfo :: HostName -> ServiceName -> IO (Socket, SockAddr)
getSendInfo host port = do
    addrinfos <- getAddrInfo Nothing (Just host) (Just port)
    case addrinfos of
        (addr:_) -> do sock <- socket (addrFamily addr) Datagram defaultProtocol
                       return $ (sock, addrAddress addr)
        _        -> do BS.putStrLn $ "Could not find address info for 31000"
                       exitFailure

--------------------------------------------------------------------------------
-- Testing
--------------------------------------------------------------------------------

runTestClient :: HostName -> ServiceName -> NominalDiffTime -> IO ()
runTestClient host port seconds = do
    t           <- getCurrentTime
    (sock,addr) <- getSendInfo host port
    g           <- newStdGen
    loop' t (randomRs (' ','~') g)
            (randomRs (1,1024) g :: [Int])
            sock
            addr
            (0 :: Int)
    where loop' t str is s a n = do
            send' (P.take (P.head is) str) s a
            t' <- getCurrentTime
            if addUTCTime seconds t <= t'
            then P.putStrLn $ "Sent " ++ (show n) ++ " messages."
            else loop' t (P.drop 100 str) (P.drop 1 is) s a (n + 1)
          send' []  _ _ = return ()
          send' msg s a = do sent <- sendTo s msg a
                             send' (P.drop sent msg) s a

linesOfFile :: MonadResource m => FilePath -> Producer m ByteString
linesOfFile file = sourceFile file $= CB.lines

runFoldFile :: FilePath -> (b -> ByteString -> b) -> b -> IO b
runFoldFile file f acc = runResourceT $ linesOfFile file $$ CL.fold f acc

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

clearMsg :: ByteString -> ByteString
clearMsg s = (BS.map clearChar s) `BS.append` "\n"

clearChar :: Char -> Char
clearChar '\n' = ' '
clearChar c    = c
