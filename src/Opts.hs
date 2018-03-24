-- | Parse cmdline options
{-# LANGUAGE TemplateHaskell #-}
module Opts
    ( Options(..), specFile
    , get
    ) where

import qualified Control.Lens as Lens
import           Data.Semigroup ((<>))
import qualified Options.Applicative as P

newtype Options = Options
    { _specFile :: FilePath
    }
Lens.makeLenses ''Options

options :: P.Parser Options
options =
    Options
    <$> P.argument P.str (P.metavar "SPECFILE")

optionsInfo :: P.ParserInfo Options
optionsInfo =
    P.info (P.helper <*> options)
    ( P.fullDesc
        <> P.progDesc "Cache a process's file outputs"
        <> P.header "git-cache - Cache the file outputs of a time consuming process"
    )

get :: IO Options
get =
    P.customExecParser (P.prefs P.showHelpOnEmpty) optionsInfo
