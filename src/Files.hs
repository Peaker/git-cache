module Files
    ( maybeMissing, copyWith, tryGetStatus
    ) where

import qualified Control.Exception as E
import           Control.Lens hiding ((<.>))
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import           System.Directory ( createDirectoryIfMissing
                                  , removeFile
                                  )
import           System.FilePath (takeDirectory)
import qualified System.IO as IO
import qualified System.IO.Error as Err
import qualified System.Posix.Files as Posix
import           System.ProgressBar.ByteString (fileReadProgressWriter)

tryGetStatus :: FilePath -> IO (Maybe Posix.FileStatus)
tryGetStatus = maybeMissing . Posix.getFileStatus

handleErrorPred :: (Err.IOErrorType -> Bool) -> IO a -> IO a -> IO a
handleErrorPred predicate handler =
    E.handle $ \e ->
    if predicate (Err.ioeGetErrorType e)
    then handler
    else E.throwIO e

handleDoesNotExist :: IO a -> IO a -> IO a
handleDoesNotExist = handleErrorPred Err.isDoesNotExistErrorType

maybeMissing :: IO a -> IO (Maybe a)
maybeMissing act = Just <$> act & handleDoesNotExist (pure Nothing)

copyMetadata :: FilePath -> FilePath -> IO ()
copyMetadata src dest =
    do
        st <- Posix.getFileStatus src
        Posix.setFileMode dest (Posix.fileMode st)
        Posix.setFileTimesHiRes dest (Posix.accessTimeHiRes st) (Posix.modificationTimeHiRes st)

copyWith :: String -> (ByteString -> ByteString) -> FilePath -> FilePath -> IO ()
copyWith prefix process srcPath destPath =
    do
        srcSt <- tryGetStatus srcPath
        destSt <- tryGetStatus destPath
        case (srcSt, destSt) of
            (Nothing, Nothing) -> return ()
            (Nothing, Just _) -> removeFile destPath
            (Just src, Just dest)
                | Posix.modificationTimeHiRes src ==
                  Posix.modificationTimeHiRes dest -> return ()
            _ ->
                do
                    createDirectoryIfMissing True (takeDirectory destPath)
                    fileReadProgressWriter srcPath IO.stdout 60 (\_ -> prefix) (\_ -> "")
                        <&> process
                        >>= BS.writeFile destPath
                    copyMetadata srcPath destPath
