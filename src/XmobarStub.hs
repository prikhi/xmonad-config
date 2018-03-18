module XmobarStub where

import Xmobar (XConf(..), startCommand, createWin, startLoop)
import Xmobar.Config (Config(..), defaultConfig)
import Xmobar.Parsers (parseTemplate)
import Xmobar.Plugins.PipeReader (PipeReader(PipeReader))
import Xmobar.Runnable (Runnable(Run))
import Xmobar.Signal (setupSignalHandler)
import Xmobar.XUtil (initFont)

import Control.Monad (void)
import Graphics.X11.Xlib (initThreads, openDisplay)
import System.IO (BufferMode(NoBuffering, LineBuffering), IOMode(ReadWriteMode), Handle, hSetBuffering, openFile, stderr)
import System.Posix (ProcessID, forkProcess)

import qualified Data.Map as Map
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
run conf = forkProcess $ do
  hSetBuffering stderr LineBuffering
  void initThreads
  d <- openDisplay ""
  fs    <- initFont d (font conf)
  fl    <- mapM (initFont d) (additionalFonts conf)
  cls   <- mapM (parseTemplate conf) (splitTemplate conf)
  sig   <- setupSignalHandler
  vars  <- mapM (mapM $ startCommand sig) cls
  (r,w) <- createWin d fs conf
  let ic = Map.empty
  startLoop (XConf d r w (fs:fl) ic conf) sig vars


-- | Splits the template in its parts. Lifted from xmobar's Main.
splitTemplate :: Config -> [String]
splitTemplate conf =
  case break (==l) t of
    (le,_:re) -> case break (==r) re of
                   (ce,_:ri) -> [le, ce, ri]
                   _         -> def
    _         -> def
  where [l, r] = alignSep
                   (if length (alignSep conf) == 2 then conf else defaultConfig)
        t = template conf
        def = [t, "", ""]
