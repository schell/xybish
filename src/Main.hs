{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Socket
import System.Exit
import Control.Applicative
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Foreign.Storable
import Data.Scientific (Scientific)
import Data.Map.Strict (Map)
import Data.Time.Clock
import Data.Time
import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M

--------------------------------------------------------------------------------
-- Client
--------------------------------------------------------------------------------
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

--------------------------------------------------------------------------------
-- Server
--------------------------------------------------------------------------------
type HandlerFunc = SockAddr -> String -> IO ()
data Server = Server Socket ThreadId

openServer :: String -> HandlerFunc -> IO Server
openServer port f = withSocketsDo $ do
    addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                             Nothing
                             (Just port)
    case addrinfos of
        (addr:_) -> do sock <- socket (addrFamily addr) Datagram defaultProtocol
                       bindSocket sock $ addrAddress addr
                       tid <- forkIO $ procMsg sock
                       return $ Server sock tid
                           where procMsg s = do (msg,_,addy) <- recvFrom s 1024
                                                f addy msg
                                                procMsg s
        _ -> do putStrLn $ "Could not find address info for" ++ port
                exitFailure

closeServer :: Server -> IO ()
closeServer (Server sock tid) = do
    killThread tid
    close sock

--------------------------------------------------------------------------------
-- XYBISH
--------------------------------------------------------------------------------
data Value = VN Scientific
           | VS ByteString
             deriving (Show, Eq)

instance Storable Value where

parseNumberValue :: Parser Value
parseNumberValue = do
    _ <- string "N"
    s <- scientific
    return $ VN s

parseStringValue :: Parser Value
parseStringValue = do
    _ <- string "S"
    s <- takeByteString
    return $ VS s

parseValue :: Parser Value
parseValue = parseStringValue <|> parseNumberValue

parseKeyString :: Parser ByteString
parseKeyString = do
    k <- takeTill isSpace
    skipSpace
    return k

parseInsertion :: Parser Insertion
parseInsertion = do
    k <- parseKeyString
    v <- parseValue
    return $ (k,v)

type Key = (UTCTime, ByteString)

type Insertion = (ByteString, Value)

data Command = CmdPrintAll

parseCommand :: Parser Command
parseCommand = do
    _ <- string "/PrintAll"
    return CmdPrintAll

doCommand :: TVar Store -> Command -> IO ()
doCommand tvar CmdPrintAll = do
    store <- atomically $ readTVar tvar
    mapM_ (\((t,k),v) -> putStrLn $ unwords [show $ utcToLocalTime utc t
                                            , B.unpack k
                                            , show v]) $ M.toList store
    putStrLn $ unwords ["Printed", show $ M.size store, "entries."]

type Store = Map Key Value

xybish :: String -> IO ()
xybish port = do
    tvar <- atomically $ newTVar M.empty
    s    <- openServer port $ \_ msg -> void $ forkIO $ do
        case parseOnly parseInsertion $ B.pack msg of
            Left err -> case parseOnly parseCommand $ B.pack msg of
                            Left err' -> putStrLn $ "Could not parse:" ++ err ++ err'
                            Right cmd -> doCommand tvar cmd
            Right (k,v) -> do t <- getCurrentTime
                              print (utcToLocalTime utc t,k,v)
                              atomically $ modifyTVar' tvar $ M.insert (t,k) v
    loop s
        where loop s = do threadDelay 1000000
                          loop s

main :: IO ()
main = putStrLn "xybish"

