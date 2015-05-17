module Tracking.Types where

import Network.Socket
--import Data.Scientific (Scientific)
--import Data.Map.Strict (Map)
--import Data.Time.Clock
--import Data.ByteString (ByteString)
--import Data.Vector.Storable

type HandlerFunc = SockAddr -> String -> IO ()
data Server = Server Socket (IO ())

data Client = Client Socket SockAddr deriving (Show, Eq)

--data Value = VN Scientific
--           | VS ByteString
--             deriving (Show, Eq)

--type Key = (UTCTime, ByteString)

--type Insertion = (ByteString, Value)

--type Store = Map Key Value
--type Store = Vector ByteString
