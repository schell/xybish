{-# LANGUAGE OverloadedStrings #-}
module Tracking.Parse where

import Tracking.Types
import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

valueToBS :: Value -> ByteString
valueToBS (VN n) = "N" `B.append` (B.pack $ show n)
valueToBS (VS s) = "S" `B.append` s

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

