module XmobarStub (run) where

import Xmobar (xmobar, Config(..), HandleReader(..), Runnable(Run))

import System.IO (BufferMode(NoBuffering), Handle, hSetBuffering)
import System.Posix (ProcessID, forkProcess)
import System.Process (createPipe)


-- | Run xmobar in a separate thread, taking input from the returned Handle.
--
-- The HandleReader is given an alias of `handle`.
run :: Config -> IO (Handle, ProcessID)
run c = do
    (readHandle, writeHandle) <- createPipe
    processId <- forkProcess $ xmobar c
        { commands =
            Run (HandleReader readHandle "handle") : commands c
        }
    hSetBuffering writeHandle NoBuffering
    return (writeHandle, processId)
