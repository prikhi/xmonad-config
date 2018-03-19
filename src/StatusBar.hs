{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module StatusBar where

import XMonad (X, ExtensionClass(..), Typeable, Event, ScreenId(S), getXMonadCacheDir, liftIO)
import XMonad.Hooks.DynamicBars (dynStatusBarStartup, dynStatusBarEventHook)
import Xmobar.Config (Config(..), XPosition(OnScreen, Top, TopP), defaultConfig)
import Xmobar.Plugins.Date (Date(Date))
import Xmobar.Plugins.Monitors (Monitors(Network, Cpu))
import Xmobar.Runnable (Runnable(Run))

import Data.Monoid (All)
import System.IO (Handle)
import System.Posix (ProcessID, signalProcess, sigTERM)

import qualified XMonad.Util.ExtensibleState as XS

import qualified Theme
import qualified XmobarStub


-- | Start the Status Bars.
startupHook :: X ()
startupHook =
    dynStatusBarStartup dynamic dynamicCleanup


-- | Restart the Status Bars on Monitor / Screen Changes
eventHook :: Event -> X All
eventHook =
    dynStatusBarEventHook dynamic dynamicCleanup


-- | Start the Status Bar for the Screen.
dynamic :: ScreenId -> X Handle
dynamic (S screenId) =
    let
        config =
            case screenId of
                0 ->
                    withTray
                1 ->
                    long
                2 ->
                    short
                _ ->
                    long
    in
        runXmobar config screenId

-- | Kill all Status Bar Processes.
dynamicCleanup :: X ()
dynamicCleanup =
    terminateProcesses


-- | Persistent Storage for the list of Status Bar ProcessIDs.
newtype StatusBarStorage
    = StatusBarStorage [ProcessID]
    deriving Typeable

instance ExtensionClass StatusBarStorage where
    initialValue = StatusBarStorage []

-- | Store the ProcessID of a Status Bar
trackProcess :: ProcessID -> X ()
trackProcess pid =
    XS.modify (\(StatusBarStorage ps) -> StatusBarStorage $ pid : ps)

-- | Kill All the Stored Status Bar Processes.
terminateProcesses :: X ()
terminateProcesses =
    XS.gets (\(StatusBarStorage ps) -> ps)
        >>= liftIO . mapM_ (signalProcess sigTERM)
        >> XS.put (StatusBarStorage [])


-- | Run xmobar on a Specific Screen & Return an Output Handle to it's
-- PipeReader.
--
-- The process ID of the bar will be added to the `StatusBarStorage` so it
-- can be killed on shutdown.
runXmobar :: Config -> Int -> X Handle
runXmobar c screenId = do
    cacheDir <- getXMonadCacheDir
    iconDir <- Theme.getIconDirectory
    let pipePath = cacheDir ++ "xmobar-" ++ show screenId ++ ".fifo"
    (handle, processId) <- liftIO $ XmobarStub.runWithPipe pipePath c
        { position = OnScreen screenId $ position c
        , iconRoot = iconDir
        }
    trackProcess processId
    return handle


-- | An Xmobar Config with just the Default Font & Colors Modified.
xmobarConfig :: Config
xmobarConfig = defaultConfig
    { font =
        Theme.xftFont
    , fgColor =
        Theme.statusBarForeground
    , bgColor =
        Theme.statusBarBackground
    }


-- | A Full-Width Status Bar for Large Screens.
long :: Config
long = xmobarConfig
    { position =
        Top
    , commands =
        [ Run $ Network "enp3s0"
            [ "-t"
            , Theme.networkUpload "<tx> KB ^" ++ Theme.networkDownload "v <rx> KB"
            ]
            25
        , Run $ Cpu
            [ "-t"
            , "<total>%"
            , "-x"
            , ""
            , "--minwidth"
            , "2"
            ]
            25
        , Run $ Date "%a %_d %b %H:%M" "date" 10
        ]
    , template = unwords
        [ "%pipe%"
        , "}{"
        , "%enp3s0%"
        , Theme.statusSeparator
        , Theme.icon Theme.CPU
        , "%cpu%"
        , Theme.statusSeparator
        , Theme.date "%date%"
        , ""
        ]
    }


-- | A Long Status Bar with 80px of Space for the System Tray.
withTray :: Config
withTray = long
    { position =
        TopP 0 80
    , template =
        template long ++ " " ++ Theme.statusSeparator
    }


-- | A Shorter Config for Monitors in Portrait Orientation.
short :: Config
short = long
    { template = unwords
        [ "%pipe%"
        , "}{"
        , Theme.date "%date%"
        , ""
        ]
    }
