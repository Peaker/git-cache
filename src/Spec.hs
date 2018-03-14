{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Spec (Manifest(..), Spec(..)) where

import           Control.Lens hiding ((<.>))
import           Data.Aeson (defaultOptions, (.:))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Lens
import           Data.Aeson.TH (deriveJSON)
import           Data.Text (Text)
import qualified Data.Text as Text

data Manifest = Manifest
    { outputFiles :: [FilePath]
    , cmd :: Text
    , cmdArgs :: [Text]
    } deriving (Eq, Show)
deriveJSON defaultOptions ''Manifest

data Spec = Spec
    { manifest :: Manifest
    , db :: FilePath
    } deriving Show
instance Aeson.ToJSON Spec where
    toJSON (Spec m d) =
        Aeson.toJSON m
        & _Object . at "db" ?~ _String # Text.pack d
instance Aeson.FromJSON Spec where
    parseJSON val =
        do
            m <- Aeson.parseJSON val
            Aeson.withObject "spec" ?? val $ \o -> Spec m <$> o .: "db"

