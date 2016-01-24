{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
module Tracking.Database where

import Database.Persist.TH
import Data.ByteString
import Data.Time.Clock

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    [persistLowerCase|
    Entry
        time UTCTime
        msg ByteString
    |]
deriving instance Show Entry
