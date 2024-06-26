{-# LANGUAGE LambdaCase #-}
module Theme where

import XMonad (X, Window, withWindowSet, cfgDir, asks, directories)
import XMonad.Hooks.DynamicLog (PP(..), xmobarColor, pad, xmobarPP)
import XMonad.Layout.IndependentScreens (unmarshallW)
import XMonad.Util.NamedWindows (getName)

import Data.List (intercalate)
import Flow

import qualified XMonad.StackSet as W


xftFont :: String
xftFont =
    [ "xft"
    , "Dina"
    , "size=8"
    , "antialias=true"
    , "autohint=0"
    , "hinting=true"
    , "hintstyle=hintfull"
    , "lcdfilter=lcddefault"
    , "rgba=rgb"
    ]
    |> intercalate ":"


-- {{{ COLORS

background :: String
background =
    "#1B1D1E"

black :: String
black =
    "#232526"

white :: String
white =
    "#F8F8F0"

grey :: String
grey =
    "#5F5F5F"

red :: String
red =
    "#FF0000"

orange :: String
orange =
    "#FD971F"

magenta :: String
magenta =
    "#F92672"

cyan :: String
cyan =
    "#66D9EF"

green :: String
green =
    "#A6E22E"

-- }}}

-- {{{ BORDERS

focusedBorder :: String
focusedBorder =
    magenta

urgentBorder :: String
urgentBorder =
    cyan

-- }}}

-- {{{ PROMPT

promptForeground :: String
promptForeground =
    magenta

promptBackground :: String
promptBackground =
    background

promptBackgroundHighlight :: String
promptBackgroundHighlight =
    background

promptForegroundHighlight :: String
promptForegroundHighlight =
    green

promptBorderColor :: String
promptBorderColor =
    magenta

-- }}}

-- {{{ XMOBAR

-- Colors

statusBarForeground :: String
statusBarForeground =
    magenta

statusBarBackground :: String
statusBarBackground =
    background

focusedTitle :: String -> String
focusedTitle =
    xmobarColor black orange

unfocusedTitle :: String -> String
unfocusedTitle =
    xmobarColor orange background

currentWorkspace :: String -> String
currentWorkspace =
    xmobarColor black magenta

urgentWorkspace :: String -> String
urgentWorkspace =
    xmobarColor black cyan

statusSeparator :: String
statusSeparator =
    icon Separator

networkUpload :: String -> String
networkUpload =
    xmobarColor green background

networkDownload :: String -> String
networkDownload =
    xmobarColor cyan background

mpdTitle :: String -> String
mpdTitle =
    xmobarColor orange background

mpdSeparator :: String
mpdSeparator =
    xmobarColor white background " - "

mpdArtist :: String -> String
mpdArtist =
    xmobarColor cyan background

mpdPaused :: String -> String
mpdPaused =
    xmobarColor magenta background

mpdStopped :: String -> String
mpdStopped =
    xmobarColor grey background

weather :: String -> String
weather =
    xmobarColor white background

date :: String -> String
date =
    xmobarColor orange background

-- Icons

getIconDirectory :: X String
getIconDirectory =
    asks $ (++ "/icons") . cfgDir . directories

data Icon
    = Separator
    | CPU
    | CurrentWorkspaceHasWindows
    | HiddenWorkspaceHasWindows
    | UrgentWorkspaceHasWindows
    | TileLayout
    | MirrorLayout
    | FullscreenLayout

icon :: Icon -> String
icon i =
    let
        name = case i of
            Separator ->
                "sep"
            CPU ->
                "cpu"
            CurrentWorkspaceHasWindows ->
                "windowcur"
            HiddenWorkspaceHasWindows ->
                "window"
            UrgentWorkspaceHasWindows ->
                "windowurg"
            TileLayout ->
                "layouttiled"
            MirrorLayout ->
                "layoutmirror"
            FullscreenLayout ->
                "layoutfullscreen"

    in
        "<icon=" ++ name ++ ".xpm/>"

-- Pretty Printers

-- | An xmobar Pretty Printer that Displays Every Window on a Workspace.
--
-- Highights the current window in the task list.
--
-- Drops the digit & underscore prefixes generated by `IndependentScreens`.
focusedScreenPP :: PP
focusedScreenPP = xmobarPP
    { ppTitle =
        const ""
    , ppVisible =
        const ""
    , ppCurrent =
        currentWorkspace . pad . unmarshallW
    , ppUrgent =
        id
    , ppSep =
        pad statusSeparator
    , ppWsSep =
        ""
    , ppLayout =
        renderLayoutIcon
    , ppExtras =
        [ logAllWindowTitles renderWindowTitle
        ]
    }
    where
        renderWindowTitle active t =
            if active == Just t then
                focusedTitle . pad . show <$> getName t
            else
                unfocusedTitle . pad . show <$> getName t
        renderLayoutIcon = \case
            "Maximize Tall" ->
                icon TileLayout
            "Maximize Mirror Tall" ->
                icon MirrorLayout
            "Full" ->
                icon FullscreenLayout
            str ->
                str

-- | Similar to the `focusedScreenPP` but with every window title de-emphasized.
unfocusedScreenPP  :: PP
unfocusedScreenPP =
    focusedScreenPP
        { ppExtras =
            [ logAllWindowTitles (const <| fmap (unfocusedTitle . pad . show) . getName) ]
        }


-- Render every Window in the WindowSet for the Pretty Printers.
logAllWindowTitles :: (Maybe Window -> Window -> X String) -> X (Maybe String)
logAllWindowTitles renderer = withWindowSet $ \ws ->
    W.index ws
        |> traverse (renderer <| W.peek ws)
        |> fmap (Just . unwords)

-- }}}
