{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, LambdaCase #-}

module Main
    ( main
    ) where

import           Codec.Compression.Lzma (compress, decompress)
import qualified Control.Exception as E
import qualified Control.Lens as Lens
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
import           System.Exit (ExitCode(..))
import           System.FilePath ((</>), (<.>))
import qualified System.IO as IO
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
manifestPath dbPath treeHash = cachePath dbPath treeHash <.> "manifest"

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

restore :: Spec -> String -> IO ()
restore spec treeHash =
    do
        compressed <- doesDirectoryExist compressedPath
        let (path, process)
                | compressed = (compressedPath, decompress)
                | otherwise = (cachePath dbPath treeHash, id)
        putStrLn $ "Restoring" ++ suffixMsg treeHash ++ ": (compressed=" ++
            show compressed ++ ", at " ++ path ++ ")"
        for_ (outputFiles (manifest spec)) $ \outputFile ->
            do
                putStrLn $ "    " ++ outputFile
                Files.copyWith "Decompressing file" process (path </> outputFile) outputFile
    where
        dbPath = db spec
        compressedPath = cachePath dbPath treeHash </> "xz"

cleanHeadTreeHash :: IO String
cleanHeadTreeHash = Git.withCleanSlate (Git.getTreeHash "HEAD")

build :: IO String -> Spec -> String -> IO ()
build getPostTreeHash spec preTreeHash =
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
        save (db spec) preTreeHash m $
            do
                postTreeHash <- getPostTreeHash
                unless (preTreeHash == postTreeHash) $
                    fail "Concurrent change and run"
    where
        m = manifest spec
        getOutputMTimes =
            traverse Files.tryGetStatus (outputFiles m)
            <&> mapped . _Just %~ Posix.modificationTimeHiRes

isCached :: Spec -> String -> IO Bool
isCached spec treeHash =
    tryParseJSON (manifestPath (db spec) treeHash)
    >>= Lens._Just %%~ validate
    <&> maybe False (const True)
    where
        validate loadedManifest =
            unless (manifest spec == loadedManifest) $ fail $
            "Mismatching spec in git-cache's db" ++ suffixMsg treeHash

bisectFail :: ExitCode
bisectFail = ExitFailure 125

run :: Opts.Options -> IO ()
run (Opts.Options specFile (Just (Opts.Load hash))) =
    do
        spec <- readJSON specFile
        treeHash <- Git.getTreeHash hash
        isCached spec treeHash
            >>= \case
            False ->
                do
                    IO.hPutStrLn IO.stderr $
                        "Uncached version requested" ++ suffixMsg treeHash
                    IO.hFlush IO.stderr
                    E.throwIO bisectFail
            True -> restore spec treeHash

run (Opts.Options specFile Nothing) =
    do
        spec <- readJSON specFile
        createDirectoryIfMissing True (db spec)
        preTreeHash <- cleanHeadTreeHash
        cached <- isCached spec preTreeHash
        (if cached then restore else build cleanHeadTreeHash) spec preTreeHash

main :: IO ()
main = Opts.get >>= run
