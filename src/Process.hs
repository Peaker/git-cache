-- | Wrap System.Process
{-# LANGUAGE LambdaCase #-}
module Process
    ( call
    , check
    , read
    ) where

import Prelude hiding (read)
import System.Exit (ExitCode(..))
import System.Process (proc, readProcess, createProcess, waitForProcess, delegate_ctlc)

call :: FilePath -> [String] -> IO ExitCode
call exe args =
    do
        (_, _, _, p) <- createProcess (proc exe args) { delegate_ctlc = True }
        waitForProcess p

check :: FilePath -> [String] -> IO ()
check exe args =
    call exe args
    >>= \case
    ExitSuccess -> return ()
    other -> fail $ unwords (exe:args) ++ " failed: " ++ show other

read :: FilePath -> [String] -> String -> IO String
read = readProcess
