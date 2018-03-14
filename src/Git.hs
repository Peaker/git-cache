-- | Run git commands
{-# LANGUAGE LambdaCase #-}
module Git
    ( getTreeHash
    ) where

import qualified Control.Exception as E
import           Control.Lens hiding ((<.>))
import           Control.Monad (unless)
import           Data.List.Split (splitOn)
import qualified Process
import           System.Exit (ExitCode(..))

callGit :: [String] -> IO ExitCode
callGit = Process.call "git"

callGitCheck :: [String] -> IO ()
callGitCheck = Process.check "git"


readGit :: [String] -> IO String
readGit args = Process.read "git" args ""

commit :: String -> [String] -> IO ExitCode
commit msg args = callGit (["commit", "-m", msg] ++ args)

uncommit :: [String] -> IO ()
uncommit args = callGitCheck (["reset", "HEAD^"] ++ args)

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

withCommit :: String -> [String] -> [String] -> IO a -> IO a
withCommit msg args uncommitArgs body =
    E.mask $ \restore ->
    commit msg args
    >>= \case
    ExitSuccess ->
        restore body `E.finally` uncommit uncommitArgs
    ExitFailure 1 ->
        -- cannot commit, possibly due to nothing to commit,
        -- proceed without uncommitting
        restore body
    other -> fail $ "git commit " ++ unwords ("-m":msg:args) ++ " failed: " ++ show other

getTreeHash :: IO String
getTreeHash =
    do
        verifyNoUntracked
        withCommit "STAGING" [] ["--soft"] $
            withCommit "WORKINGTREE" ["-a"] [] $
            do
                ["tree", treehash] <-
                    readGit ["cat-file", "-p", "HEAD"]
                    <&> lines <&> head <&> words
                pure treehash
