module XmobarStub (run, terminateProcesses) where

import XMonad (X, ExtensionClass(..), Typeable, liftIO)
import Xmobar (xmobar, Config(..), HandleReader(..), Runnable(Run))

import System.IO (BufferMode(NoBuffering), Handle, hSetBuffering, hClose)
import System.Posix (ProcessID, forkProcess, signalProcess, sigTERM)
import System.Process (createPipe)

import qualified XMonad.Util.ExtensibleState as XS


-- | Run xmobar in a separate thread, taking input from the returned Handle.
--
-- The HandleReader is given an alias of `handle`.
run :: Config -> X Handle
run c = do
    d@(_, _, writeHandle) <- liftIO $ do
        (readHandle, writeHandle) <- createPipe
        processId <- forkProcess $ xmobar c
            { commands =
                Run (HandleReader readHandle "handle") : commands c
            }
        hSetBuffering writeHandle NoBuffering
        return (processId, readHandle, writeHandle)
    trackProcess d
    return writeHandle


-- Process & Handle Management

type StorageData = (ProcessID, Handle, Handle)

-- | Persistent Storage for the list of Status Bar ProcessIDs.
newtype StatusBarStorage
    = StatusBarStorage [StorageData]
    deriving Typeable

instance ExtensionClass StatusBarStorage where
    initialValue = StatusBarStorage []

-- | Store the ProcessID of a Status Bar
trackProcess :: StorageData -> X ()
trackProcess x =
    XS.modify (\(StatusBarStorage xs) -> StatusBarStorage $ x : xs)

-- | Stop all the xmobar processes and close their communication Handles.
terminateProcesses :: X ()
terminateProcesses =
    XS.gets (\(StatusBarStorage ps) -> ps)
        >>= liftIO . mapM_ terminate
        >> XS.put (StatusBarStorage [])
  where
    terminate (pId, readHandle, writeHandle) =
        signalProcess sigTERM pId >> hClose writeHandle >> hClose readHandle
