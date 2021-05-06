-- | Parse cmdline options
{-# LANGUAGE TemplateHaskell #-}
module Opts
    ( Load(..), loadForGitHash
    , Options(..), specFile, load
    , get
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import qualified Options.Applicative as P

newtype Load = Load
    { _loadForGitHash :: String
    }
Lens.makeLenses ''Load

data Options = Options
    { _specFile :: FilePath
    , _load :: Maybe Load
    }
Lens.makeLenses ''Options

parseStr :: P.Mod P.ArgumentFields String -> P.Parser String
parseStr = P.argument P.str

parseSpecfile :: P.Parser String
parseSpecfile = parseStr (P.metavar "SPECFILE")

parseLoad :: P.Parser Load
parseLoad =
    P.strOption
    ( P.metavar "REFSPEC"
      <> P.long "load"
      <> P.short 'l'
      <> P.help "Load (don't build) from a specified git hash"
    ) <&> Load

parseOptions :: P.Parser Options
parseOptions = Options <$> parseSpecfile <*> P.optional parseLoad

parseOptionsInfo :: P.ParserInfo Options
parseOptionsInfo =
    P.info (P.helper <*> parseOptions)
    ( P.progDesc "Cache a process's file outputs"
        <> P.header "git-cache - Cache the file outputs of a time consuming process"
    )

get :: IO Options
get =
    P.customExecParser (P.prefs P.showHelpOnEmpty) parseOptionsInfo
