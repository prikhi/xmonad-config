{-# LANGUAGE OverloadedStrings #-}
module StatusBar (startupHook, eventHook, stopHook) where

import XMonad (X, Event, ScreenId(S), spawn)
import XMonad.Hooks.DynamicBars (dynStatusBarStartup, dynStatusBarEventHook)
import Xmobar
    ( Config(..), XPosition(OnScreen, Top, TopP), Date(Date)
    , Monitors(Network, Cpu, Weather, MPD), Runnable(Run), defaultConfig
    )

import Control.Monad (when)
import Data.Monoid (All)
import System.IO (Handle)


import qualified Theme
import qualified XmobarStub


-- Hooks

-- | Start the Status Bars & System Tray.
startupHook :: X ()
startupHook =
    dynStatusBarStartup dynamic dynamicCleanup


-- | Restart the Status Bars & System Tray on Monitor / Screen Changes
eventHook :: Event -> X All
eventHook =
    dynStatusBarEventHook dynamic dynamicCleanup

-- | Stop the Status Bar & System Tray Processes.
stopHook :: X ()
stopHook =
    dynamicCleanup


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
                    blank
    in
        when (screenId == 0) startSystemTray
            >> runXmobar config screenId

-- | Kill all Status Bar & System Tray Processes.
dynamicCleanup :: X ()
dynamicCleanup =
    XmobarStub.terminateProcesses
        >> stopSystemTray


-- System Tray

-- | The Desired Width of System Tray in Pixels.
systemTrayWidth :: Int
systemTrayWidth =
    100

-- | Start the System Tray Process.
startSystemTray :: X ()
startSystemTray =
    let
        args =
            [ "--edge", "top"
            , "--align", "right"
            , "--width", show systemTrayWidth
            , "--widthtype", "pixels"
            , "--expand", "false"
            , "--monitor", "primary"
            , "--height", "17"
            , "--tint", "0x" ++ drop 1 Theme.background
            , "--alpha", "0"
            , "--transparent", "true"
            ]
    in
        spawn $ "sleep 0.1; trayer " ++ unwords args

-- | Stop the System Tray Process.
stopSystemTray :: X ()
stopSystemTray =
    spawn "pkill trayer"


-- Xmobar

-- | Run xmobar on a Specific Screen & Return a Write Handle to it's
-- HandleReader.
--
-- The process ID of the bar will be added to the `StatusBarStorage` so it
-- can be killed on shutdown.
runXmobar :: Config -> Int -> X Handle
runXmobar c screenId = do
    iconDir <- Theme.getIconDirectory
    XmobarStub.run c
        { position = OnScreen screenId $ position c
        , iconRoot = iconDir
        }


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
        [ Run $ MPD
            [ "-t", "<statei>"
            , "--"
            , "-Z", Theme.mpdPaused "mpd paused"
            , "-S", Theme.mpdStopped "mpd stopped"
            , "-P", Theme.mpdTitle "<title>" ++ Theme.mpdSeparator ++ Theme.mpdArtist "<artist>"
            ] 10
        , Run $ Network "enp0s25"
            [ "-t"
            , Theme.networkUpload "<tx> KB ^" ++ Theme.networkDownload "v <rx> KB"
            ]
            25
        , Run $ Weather "K7W4"
            [ "-t"
            , "<tempF>°"
            ]
            3000
        , Run $ Cpu
            [ "-t"
            , "<total>%"
            , "-x"
            , ""
            , "--minwidth"
            , "2"
            ]
            25
        , Run $ Date "%a %d %b %H:%M" "date" 10
        ]
    , template = unwords
        [ "%handle%"
        , "}{"
        , "%mpd%"
        , Theme.statusSeparator
        , "%enp0s25%"
        , Theme.statusSeparator
        , Theme.icon Theme.CPU
        , "%cpu%"
        , Theme.statusSeparator
        , Theme.weather "%K7W4%"
        , Theme.statusSeparator
        , Theme.date "%date%"
        , ""
        ]
    }


-- | A Long Status Bar with Space for the System Tray.
withTray :: Config
withTray = long
    { position =
        TopP 0 systemTrayWidth
    , template =
        template long ++ " " ++ Theme.statusSeparator
    }


-- | A Shorter Config for Monitors in Portrait Orientation.
short :: Config
short = long
    { template = unwords
        [ "%handle%"
        , "}{"
        , Theme.date "%date%"
        , ""
        ]
    }

blank :: Config
blank = long { commands = [], template = "" }
