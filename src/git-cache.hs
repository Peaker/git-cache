{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell, LambdaCase #-}

module Main
    ( main
    ) where

import           Codec.Compression.Lzma (compress, decompress)
import qualified Control.Exception as E
import           Control.Lens hiding ((<.>))
import           Control.Monad (unless)
import           Data.Aeson (defaultOptions, (.:))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Lens
import           Data.Aeson.TH (deriveJSON)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import           Data.Foldable (for_)
import           Data.List.Split (splitOn)
import           Data.Text (Text)
import qualified Data.Text as Text
import           System.Directory ( createDirectoryIfMissing
                                  , removePathForcibly
                                  , doesDirectoryExist
                                  , removeFile
                                  )
import           System.Environment (getArgs)
import           System.FilePath ((</>), (<.>), takeDirectory)
import qualified System.IO as IO
import qualified System.IO.Error as Err
import qualified System.Posix.Files as Posix
import           System.Process (callProcess, readProcess)
import           System.ProgressBar.ByteString (fileReadProgressWriter)

callGit :: [String] -> IO ()
callGit = callProcess "git"

readGit :: [String] -> IO String
readGit args = readProcess "git" args ""

commit :: String -> [String] -> IO ()
commit msg args = callGit (["commit", "-m", msg, "--allow-empty"] ++ args)

uncommit :: [String] -> IO ()
uncommit args = callGit (["reset", "HEAD^"] ++ args)

gitStatus :: IO String
gitStatus = readGit ["status", "--porcelain", "-z"]

verifyNoUntracked :: IO ()
verifyNoUntracked =
    do
        untrackedFiles <-
            gitStatus
            <&> splitOn "\NUL"
            <&> filter (("?? "==) . take 3)
            <&> map (drop 3)
        unless (null untrackedFiles) $
            fail ("Have untracked files: " ++ show untrackedFiles)

getTreeHash :: IO String
getTreeHash =
    do
        verifyNoUntracked
        E.bracket_ (commit "STAGING" []) (uncommit ["--soft"]) $
            E.bracket_ (commit "WORKINGTREE" ["-a"]) (uncommit []) $
            do
                ["tree", treehash] <-
                    readGit ["cat-file", "-p", "HEAD"]
                    <&> lines <&> head <&> words
                pure treehash

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

handleErrorPred :: (Err.IOErrorType -> Bool) -> IO a -> IO a -> IO a
handleErrorPred predicate handler =
    E.handle $ \e ->
    if predicate (Err.ioeGetErrorType e)
    then handler
    else E.throwIO e

handleDoesNotExist :: IO a -> IO a -> IO a
handleDoesNotExist = handleErrorPred Err.isDoesNotExistErrorType

maybeMissingFile :: IO a -> IO (Maybe a)
maybeMissingFile act = Just <$> act & handleDoesNotExist (pure Nothing)

readJSON :: Aeson.FromJSON a => FilePath -> IO a
readJSON path =
    do
        bs <- BS.readFile path
        Aeson.eitherDecode' bs & either (fail . show) pure

tryGetFileStatus :: FilePath -> IO (Maybe Posix.FileStatus)
tryGetFileStatus = maybeMissingFile . Posix.getFileStatus

tryParseJSON :: Aeson.FromJSON a => FilePath -> IO (Maybe a)
tryParseJSON = maybeMissingFile . readJSON

cachePath :: FilePath -> String -> FilePath
cachePath dbPath hash = dbPath </> hash

manifestPath :: FilePath -> String -> FilePath
manifestPath dbPath preTreeHash = cachePath dbPath preTreeHash <.> "manifest"

suffixMsg :: String -> String
suffixMsg hash = " (on tree " ++ hash ++ ")"

copyFileMetadata :: FilePath -> FilePath -> IO ()
copyFileMetadata src dest =
    do
        st <- Posix.getFileStatus src
        Posix.setFileMode dest (Posix.fileMode st)
        Posix.setFileTimesHiRes dest (Posix.accessTimeHiRes st) (Posix.modificationTimeHiRes st)

-- | copyFileWithMetadata but also create output dir as needed, or delete if needd
copy :: String -> (ByteString -> ByteString) -> FilePath -> FilePath -> IO ()
copy prefix process srcPath destPath =
    do
        srcSt <- tryGetFileStatus srcPath
        destSt <- tryGetFileStatus destPath
        case (srcSt, destSt) of
            (Nothing, Nothing) -> return ()
            (Nothing, Just _) -> removeFile destPath
            (Just src, Just dest)
                | Posix.modificationTimeHiRes src ==
                  Posix.modificationTimeHiRes dest -> return ()
            _ ->
                do
                    createDirectoryIfMissing True (takeDirectory destPath)
                    fileReadProgressWriter srcPath IO.stdout 60 (\_ _ -> prefix) (\_ _ -> "")
                        <&> process
                        >>= BS.writeFile destPath
                    copyFileMetadata srcPath destPath

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
                copy "Compressing file..." compress outputFile destPath
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
                copy "Decompressing file" process (path </> outputFile) outputFile
    where
        compressedPath = cachePath dbPath preTreeHash </> "xz"

build :: FilePath -> String -> Manifest -> IO ()
build dbPath preTreeHash m =
    do
        preMTimes <- getOutputMTimes
        putStrLn $
            "Executing " ++ unwords (cmd m : cmdArgs m <&> Text.unpack)
            ++ suffixMsg preTreeHash
        callProcess (cmd m & Text.unpack) (cmdArgs m <&> Text.unpack)
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
                postTreeHash <- getTreeHash
                unless (preTreeHash == postTreeHash) $
                    fail "Concurrent change and run"
    where
        getOutputMTimes =
            traverse tryGetFileStatus (outputFiles m)
            <&> mapped . _Just %~ Posix.modificationTimeHiRes

run :: FilePath -> IO ()
run specFile =
    do
        spec <- readJSON specFile
        createDirectoryIfMissing True (db spec)
        let m = manifest spec
        preTreeHash <- getTreeHash
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
        [specFile] <- getArgs
        run specFile
