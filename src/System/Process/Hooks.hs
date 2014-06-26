{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module System.Process.Hooks
  ( HooksSpec()
  , defaultHooksSpec
  , inDir
  , withArg
  , withEnv
  , withWorkingDir
  , withStdOut
  , withStdIn
  , withStdErr
  , recurseDir
  , noRecurseDir
  , closeFDs
  , noCloseFDs
  , ProcessData(..)
  , pHandle
  , runHooks
  , runHooksInDir
  , runAndWaitForHooksInDir
  , waitForHooks
  , readStdErr
  , readStdOut
  , StdStream(..)
  ) where

import Control.Arrow ((***))
import Control.Applicative ((<$>))
import Control.Monad (filterM, forM)
import Data.Text (Text)
import System.Directory ( getDirectoryContents, getPermissions, executable
                        , doesDirectoryExist)
import System.IO (Handle)
import System.Process ( CreateProcess(cwd, env, close_fds, std_in, std_out, std_err, create_group), ProcessHandle, StdStream(Inherit)
                      , createProcess, proc, waitForProcess)
import System.Exit (ExitCode())

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data HooksSpec = HooksSpec
  { hDirs :: [(RecurseFlag, FilePath)]
  , hArguments :: [Text]
  -- ^ list of arguments for the new processes
  , hCWD :: Maybe FilePath
  -- ^ the working directory
  , hEnv :: [(Text, Text)]
  -- ^ the environment
  , hCloseFDs :: Bool
  -- ^ close all file descriptors except stdin, stdout and stderr
  , hCreateGroup :: Bool
  -- ^ create a new process group
  -- , pParallel :: ParallelFlag
  --  whether to run the executables in parallel
  , hStdIn :: StdStream
  -- ^ the stdin stream
  , hStdOut :: StdStream
  -- ^ the stdout stream
  , hStdErr :: StdStream
  -- ^ the stderr stream
  }

data RecurseFlag = Recursive | NonRecursive
  deriving (Show, Read, Eq, Ord, Bounded)

-- | Set the directory in which the hooks are to be found.
-- This is the same as 'noRecurse'
inDir :: FilePath -> HooksSpec -> HooksSpec
inDir = noRecurseDir

withEnv :: Text -> Text -> HooksSpec -> HooksSpec
withEnv k v s = s { hEnv = (k,v) : hEnv s }

withArg :: Text -> HooksSpec -> HooksSpec
withArg a s = s { hArguments = a : hArguments s }

withWorkingDir :: FilePath -> HooksSpec -> HooksSpec
withWorkingDir d s = s { hCWD = Just d }

closeFDs :: HooksSpec -> HooksSpec
closeFDs s = s { hCloseFDs = True}

noCloseFDs :: HooksSpec -> HooksSpec
noCloseFDs s = s { hCloseFDs = True}

noRecurseDir :: FilePath -> HooksSpec -> HooksSpec
noRecurseDir f s = s { hDirs = (NonRecursive, f) : hDirs s }

recurseDir :: FilePath -> HooksSpec -> HooksSpec
recurseDir f s = s { hDirs = (Recursive, f) : hDirs s }

withStdOut :: StdStream -> HooksSpec -> HooksSpec
withStdOut stream s = s { hStdOut = stream }

withStdErr :: StdStream -> HooksSpec -> HooksSpec
withStdErr stream s = s { hStdErr = stream }

withStdIn :: StdStream -> HooksSpec -> HooksSpec
withStdIn stream s = s { hStdIn = stream }

-- data ParallelFlag = Parallel | Sequential
--   deriving (Show, Read, Eq, Ord, Bounded)

-- | Default 'HooksSpec' that will not find any hooks. Be sure to add
-- a directory with 'inDir', 'noRecurseDir' or 'recurseDir'.
defaultHooksSpec :: HooksSpec
defaultHooksSpec = HooksSpec [] [] Nothing [] True False
                      Inherit Inherit Inherit

data ProcessData = ProcessData
  { pName :: FilePath
  , pData :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
  }

pHandle :: ProcessData -> ProcessHandle
pHandle (ProcessData _ (_, _, _, h)) = h

-- | Create all exectuables in the directory specified by the
runHooks :: HooksSpec -> IO (Either String [ProcessData])
runHooks HooksSpec{..} = do
    executables <- concat <$> mapM (uncurry getExecutables) hDirs
    results <- forM executables $ \exe -> do
        let spec = (proc exe (map T.unpack hArguments))
                        { cwd = hCWD
                        , env = Just $ map (T.unpack *** T.unpack) hEnv
                        , std_in = hStdIn
                        , std_out = hStdOut
                        , std_err = hStdErr
                        , close_fds = hCloseFDs
                        , create_group = hCreateGroup
                        }
        ProcessData exe <$> createProcess spec
    return . Right $ results

getExecutables :: RecurseFlag -> FilePath -> IO [FilePath]
getExecutables NonRecursive dir =
    filterM (fmap executable . getPermissions) =<< dirContents dir
getExecutables Recursive dir = do
    dirC <- dirContents dir
    executables <- filterM (fmap executable . getPermissions) dirC
    subdirs <- filterM doesDirectoryExist dirC
    concat . (executables :) <$>
                 mapM (getExecutables Recursive) subdirs

dirContents :: FilePath -> IO [FilePath]
dirContents dir = map ((dir ++ "/") ++) . filter (not . ('.' ==) . head) <$>
                      getDirectoryContents dir

-- | Run all hooks in the directory with the given arguments.
-- See 'defaultHooksSpec' for other configuration.
runHooksInDir :: FilePath -> [Text] -> IO (Either String [ProcessData])
runHooksInDir dir args = runHooks spec
  where spec = noRecurseDir dir $ defaultHooksSpec { hArguments = args }

-- | Wait for all hooks to finish running.
waitForHooks :: [ProcessData] -> IO [ExitCode]
waitForHooks = mapM (waitForProcess . pHandle)

-- | Run hooks in directory with given arguments and wait for completion.
-- This is the straightforward combination of 'runHooksInDir' and
-- 'waitForHooks'.
runAndWaitForHooksInDir :: FilePath -> [Text] -> IO (Either String [ExitCode])
runAndWaitForHooksInDir dir args =
    runHooksInDir dir args >>= \case
        Left err -> return $ Left err
        Right pd -> Right <$> waitForHooks pd

-- | Read stdout from processdata waiting for the process to exit.
-- It will yield an empty string if no stdout handle is given.
readStdOut :: ProcessData -> IO Text
readStdOut (ProcessData _ (_, _, Just stdout, handle)) =
    waitForProcess handle >> TIO.hGetContents stdout
readStdOut _ = return T.empty

-- | Read stderr from processdata waiting for the process to exit.
-- It will yield an empty string if no stderr handle is given.
readStdErr :: ProcessData -> IO Text
readStdErr (ProcessData _ (_, _, Just stderr, handle)) =
    waitForProcess handle >> TIO.hGetContents stderr
readStdErr _ = return T.empty
