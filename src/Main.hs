{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, LambdaCase #-}

module Main
    ( main
    ) where

import           Codec.Compression.Lzma (compress, decompress)
import qualified Control.Exception as E
import           Control.Lens hiding ((<.>))
import           Control.Monad (unless)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BS
import           Data.Foldable (for_)
import qualified Data.Text as Text
import qualified Files
import qualified Git
import qualified Opts
import qualified Process
import           Spec (Manifest(..), Spec(..))
import           System.Directory ( createDirectoryIfMissing
                                  , removePathForcibly
                                  , doesDirectoryExist
                                  )
import           System.FilePath ((</>), (<.>))
import qualified System.Posix.Files as Posix

readJSON :: Aeson.FromJSON a => FilePath -> IO a
readJSON path =
    do
        bs <- BS.readFile path
        Aeson.eitherDecode' bs & either (fail . show) pure

tryParseJSON :: Aeson.FromJSON a => FilePath -> IO (Maybe a)
tryParseJSON = Files.maybeMissing . readJSON

cachePath :: FilePath -> String -> FilePath
cachePath dbPath hash = dbPath </> hash

manifestPath :: FilePath -> String -> FilePath
manifestPath dbPath preTreeHash = cachePath dbPath preTreeHash <.> "manifest"

suffixMsg :: String -> String
suffixMsg hash = " (on tree " ++ hash ++ ")"

save :: FilePath -> String -> Manifest -> IO a -> IO a
save dbPath preTreeHash m act =
    cleanOnError $
    do
        putStrLn $ "Saving" ++ suffixMsg preTreeHash
        createDirectoryIfMissing True (cachePath dbPath preTreeHash)
        -- write the manifest
        BS.writeFile (manifestPath dbPath preTreeHash) (Aeson.encode m)
        -- copy the output files
        for_ (outputFiles m) $ \outputFile ->
            do
                let destPath = cachePath dbPath preTreeHash </> "xz" </> outputFile
                Files.copyWith "Compressing file..." compress outputFile destPath
        act
    where
        cleanOnError = (`E.onException` cleanup)
        cleanup =
            do
                putStrLn $ "Cleaning partially saved" ++ suffixMsg preTreeHash
                for_ [cachePath dbPath preTreeHash, manifestPath dbPath preTreeHash] $
                    \path ->
                        do
                            putStrLn $ "Cleaning: " ++ path
                            removePathForcibly path

restore :: FilePath -> String -> Manifest -> IO ()
restore dbPath preTreeHash m =
    do
        compressed <- doesDirectoryExist compressedPath
        let (path, process)
                | compressed = (compressedPath, decompress)
                | otherwise = (cachePath dbPath preTreeHash, id)
        putStrLn $ "Restoring" ++ suffixMsg preTreeHash ++ ": (compressed=" ++
            show compressed ++ ", at " ++ path ++ ")"
        for_ (outputFiles m) $ \outputFile ->
            do
                putStrLn $ "    " ++ outputFile
                Files.copyWith "Decompressing file" process (path </> outputFile) outputFile
    where
        compressedPath = cachePath dbPath preTreeHash </> "xz"

cleanHeadTreeHash :: IO String
cleanHeadTreeHash = Git.withCleanSlate (Git.getTreeHash "HEAD")

build :: FilePath -> String -> Manifest -> IO ()
build dbPath preTreeHash m =
    do
        preMTimes <- getOutputMTimes
        putStrLn $
            "Executing " ++ unwords (cmd m : cmdArgs m <&> Text.unpack)
            ++ suffixMsg preTreeHash
        Process.check (cmd m & Text.unpack) (cmdArgs m <&> Text.unpack)
        postMTimes <- getOutputMTimes
        for_ (zip3 (outputFiles m) preMTimes postMTimes) $
            \case
            (outputFile, Just preMTime, Just postMTime)
                | preMTime == postMTime ->
                      putStrLn $
                      "WARNING: " ++ show outputFile ++ " was not touched"
            _ -> pure ()
        save dbPath preTreeHash m $
            do
                postTreeHash <- cleanHeadTreeHash
                unless (preTreeHash == postTreeHash) $
                    fail "Concurrent change and run"
    where
        getOutputMTimes =
            traverse Files.tryGetStatus (outputFiles m)
            <&> mapped . _Just %~ Posix.modificationTimeHiRes

run :: FilePath -> IO ()
run specFile =
    do
        spec <- readJSON specFile
        createDirectoryIfMissing True (db spec)
        let m = manifest spec
        preTreeHash <- cleanHeadTreeHash
        mOldManifest <- tryParseJSON (manifestPath (db spec) preTreeHash)
        action <-
            case mOldManifest of
            Nothing -> pure build
            Just oldManifest ->
                do
                    unless (oldManifest == m) $
                        fail $
                        "Different spec at execution time and at "
                        ++ "original caching time" ++ suffixMsg preTreeHash
                    pure restore
        action (db spec) preTreeHash m

main :: IO ()
main =
    do
        Opts.Options specFile <- Opts.get
        run specFile
