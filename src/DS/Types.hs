{-# LANGUAGE OverloadedStrings #-}
module DS.Types
  ( Command
  , command
  , DSConfig (..)
  , emptyDSConfig
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Yaml           ((.!=), (.:?), (.=))
import qualified Data.Yaml           as Y

newtype Command = Command { command :: String }
    deriving Show

instance Y.ToJSON Command where
    toJSON = Y.toJSON . command

instance Y.FromJSON Command where
    parseJSON v = Command <$> Y.parseJSON v

data DSConfig = DSConfig
    { dsInit         :: !(Maybe Command)
    , dsTestData     :: !(Maybe Command)
    , dsBackup       :: !(Maybe Command)
    , dsRestore      :: !(Maybe Command)
    , dsRemove       :: !(Maybe Command)
    , dsDataLocation :: ![String]
    } deriving Show

emptyDSConfig :: DSConfig
emptyDSConfig = DSConfig emptyCommand emptyCommand emptyCommand emptyCommand emptyCommand []
  where
    emptyCommand = Just $ Command ""

instance Y.ToJSON DSConfig where
    toJSON config = Y.object
        [ "init"          .= dsInit config
        , "test-data"     .= dsTestData config
        , "backup"        .= dsBackup config
        , "restore"       .= dsRestore config
        , "remove"        .= dsRemove config
        , "data-location" .= dsDataLocation config
        ]

instance Y.FromJSON DSConfig where
    parseJSON = Y.parseJSON >=> go
      where
        go o = DSConfig
            <$> o .:? "init"
            <*> o .:? "test-data"
            <*> o .:? "backup"
            <*> o .:? "restore"
            <*> o .:? "remove"
            <*> o .:? "data-location" .!= []
