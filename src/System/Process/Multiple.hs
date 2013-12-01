{-# LANGUAGE RecordWildCards #-}
module System.Process.Multiple
  ( ProcessSpec(..)
  , CloseFDsFlag(..)
  , RecurseFlag(..)
  , ProcessData(..)
  , pHandle
  , createAllProcesses
  , runAllCommands
  , waitForAll
  , readStdErr
  , readStdOut
  , StdStream(..)
  ) where

import Control.Arrow ((***))
import Control.Applicative ((<$>))
import Control.Monad (filterM, forM, liftM)
import Control.Monad.IO.Class (MonadIO(), liftIO)
import Data.Default (Default, def)
import Data.Text (Text)
import System.Directory ( getDirectoryContents, getPermissions, executable
                        , doesDirectoryExist)
import System.IO (Handle)
import System.Process ( CreateProcess(..), ProcessHandle, StdStream(Inherit)
                      , createProcess, proc, waitForProcess)
import System.Exit (ExitCode())

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data ProcessSpec = ProcessSpec
  { pDir :: FilePath
  -- ^ The directory in which the executables are to be found
  , pArguments :: [Text]
  -- ^ list of arguments for the new processes
  , pCWD :: Maybe FilePath
  -- ^ the working directory
  , pEnv :: Maybe [(Text, Text)]
  -- ^ the environment
  , pCloseFDs :: CloseFDsFlag
  -- ^ close all file descriptors except stdin, stdout and stderr
  , pCreateGroup :: CreateGroupFlag
  -- ^ create a new process group
  , pRecurse :: RecurseFlag
  -- ^ whether to recurse into subdirectories
  -- , pParallel :: ParallelFlag
  -- ^ whether to run the executables in parallel
  , pStdIn :: StdStream
  -- ^ the stdin stream
  , pStdOut :: StdStream
  -- ^ the stdout stream
  , pStdErr :: StdStream
  -- ^ the stderr stream
  }

data CloseFDsFlag = CloseFDs | DontCloseFDs
  deriving (Show, Read, Eq, Ord, Bounded)

data CreateGroupFlag = CreateGroup | DontCreateGroup
  deriving (Show, Read, Eq, Ord, Bounded)

data RecurseFlag = Recursive | NonRecursive
  deriving (Show, Read, Eq, Ord, Bounded)

-- data ParallelFlag = Parallel | Sequential
--   deriving (Show, Read, Eq, Ord, Bounded)

instance Default ProcessSpec where
    def = ProcessSpec "/" [] Nothing Nothing CloseFDs DontCreateGroup NonRecursive
                      Inherit Inherit Inherit

data ProcessData = ProcessData
  { pName :: FilePath
  , pData :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
  }

pHandle :: ProcessData -> ProcessHandle
pHandle (ProcessData _ (_, _, _, h)) = h

-- | Create all exectuables in the directory specified by the
createAllProcesses :: MonadIO io
  => ProcessSpec
  -> io (Either String [ProcessData])
createAllProcesses ProcessSpec{..} = liftIO $ do
    executables <- getExecutables pRecurse pDir
    results <- forM executables $ \exe -> do
        let spec = (proc exe (map T.unpack pArguments))
                        { cwd = pCWD
                        , env = map (T.unpack *** T.unpack) <$> pEnv
                        , std_in = pStdIn
                        , std_out = pStdOut
                        , std_err = pStdErr
                        , close_fds = case pCloseFDs of
                                        CloseFDs -> True
                                        DontCloseFDs -> False
                        , create_group = case pCreateGroup of
                                           CreateGroup -> True
                                           DontCreateGroup -> False
                        }
        liftM (ProcessData exe) $ createProcess spec
    return . Right $ results

getExecutables :: MonadIO io => RecurseFlag -> FilePath -> io [FilePath]
getExecutables NonRecursive dir = liftIO $
    filterM (fmap executable . getPermissions) =<< dirContents dir
getExecutables Recursive dir = liftIO $ do
    dirC <- dirContents dir
    executables <- filterM (fmap executable . getPermissions) dirC
    subdirs <- filterM doesDirectoryExist dirC
    concat . (executables :) <$>
                 mapM (getExecutables Recursive) subdirs

dirContents :: FilePath -> IO [FilePath]
dirContents dir = map ((dir ++ "/") ++) . filter (not . ('.' ==) . head) <$>
                      getDirectoryContents dir

runAllCommands :: MonadIO io
  => FilePath -> [Text] -> io (Either String [ProcessData])
runAllCommands dir args = createAllProcesses spec
  where spec = def { pDir = dir, pArguments = args }

waitForAll :: MonadIO io => [ProcessData] -> io [ExitCode]
waitForAll = mapM (liftIO . waitForProcess . pHandle)

-- | read stdout from processdata waiting for the process to exit
-- will yield the empty string if no stdout handle is given
readStdOut :: MonadIO io => ProcessData -> io Text
readStdOut (ProcessData _ (_, _, Just stdout, handle)) = liftIO $
    waitForProcess handle >> TIO.hGetContents stdout
readStdOut _ = return T.empty

-- | read stderr from processdata waiting for the process to exit
-- will yield the empty string if no stderr handle is given
--
readStdErr :: MonadIO io => ProcessData -> io Text
readStdErr (ProcessData _ (_, _, Just stderr, handle)) = liftIO $
    waitForProcess handle >> TIO.hGetContents stderr
readStdErr _ = return T.empty
