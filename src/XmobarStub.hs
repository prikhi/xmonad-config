module XmobarStub where

import Xmobar (xmobar, Config(..), PipeReader(PipeReader), Runnable(Run))

import System.IO (BufferMode(NoBuffering), IOMode(ReadWriteMode), Handle, hSetBuffering, openFile)
import System.Posix (ProcessID, forkProcess)

import qualified System.Posix.Files as F


-- | Run xmobar in a separate thread, taking input from a POSIX named pipe.
--
-- The PipeReader is given an alias of `pipe`.
runWithPipe :: FilePath -> Config -> IO (Handle, ProcessID)
runWithPipe pipePath c = do
    let pipeAlias = "pipe"
        loadingText = "Initializing..."
    createPipe pipePath
    processId <- run c
        { commands =
            -- TODO: Custom `HandleReader` xmobar plugin?
            Run (PipeReader (loadingText ++ ":" ++ pipePath) pipeAlias) : commands c
        }
    outputHandle <- openFile pipePath ReadWriteMode
    hSetBuffering outputHandle NoBuffering
    return (outputHandle, processId)
    where createPipe path = do
            exists <- F.fileExist path
            if exists then
                F.setFileMode pipePath $ foldl F.unionFileModes F.namedPipeMode
                    [ F.ownerReadMode
                    , F.ownerWriteMode
                    ]
            else
                F.createNamedPipe pipePath $
                    F.ownerReadMode `F.unionFileModes` F.ownerWriteMode

-- | Run xmobar using the given config. Lifted from xmobar's Main.
run :: Config -> IO ProcessID
run = forkProcess . xmobar
