module Config where


import XMonad
import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog

import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Actions.Navigation2D (withNavigation2DConfig, windowGo, windowSwap, Direction2D(..))
import XMonad.Actions.OnScreen (viewOnScreen)
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks (docks, avoidStruts)
import XMonad.Hooks.ManageHelpers (isInProperty)
import XMonad.Hooks.DynamicBars (multiPPFormat)
import XMonad.Hooks.FadeInactive (fadeOutLogHook, isUnfocused)
import XMonad.Layout.IndependentScreens
    ( PhysicalWorkspace, countScreens, withScreens, onCurrentScreen, workspaces'
    , marshall, unmarshallW, unmarshallS
    )
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.Maximize (maximizeWithPadding, maximizeRestore)
import XMonad.Layout.PerScreen (ifWider)
import XMonad.Prompt (XPConfig(..), XPPosition(..))
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Util.SpawnOnce (spawnOnce)

import Control.Monad ((>=>))
import Data.List (isPrefixOf)
import Data.Monoid (All)
import Flow
import System.Exit (exitSuccess)

import qualified Data.Map as Map
import qualified XMonad.StackSet as W

import qualified Theme
import qualified StatusBar


-- {{{ CONFIGURATION

-- | Super simple for now, will extend w/ AwesomeWM features I like.
--
-- Starts a xmobar on the primary monitor & feeds it the status bar text.
myConfig = do
    screenCount_ <- countScreens
    def { terminal =
            "urxvtc"
        , modMask =
            mod4Mask
        , focusFollowsMouse =
            True
        , normalBorderColor =
            Theme.background
        , focusedBorderColor =
            Theme.focusedBorder
        , layoutHook =
            myLayoutHook
        , startupHook =
            myStartupHook
        , logHook =
            myLogHook
        , workspaces =
            myWorkspaces screenCount_
        , manageHook =
            myManageHook
        , handleEventHook =
            myEventHook <+> handleEventHook def
        , keys =
            myKeys
        }
        |> withNavigation2DConfig def
        |> ewmh
        |> ewmhFullscreen
        |> docks
        |> (return :: a -> IO a)

-- }}}



-- {{{ LAYOUTS

-- | Prevent tiled windows from overlapping status bar & remove borders
-- from the fullscreen layout.
myLayoutHook =
    tiledLayouts ||| fullscreenBorderless
    where
        tiledLayouts =
            ifWider 1050 (tiled ||| Mirror tiled) (Mirror tiled ||| tiled)
                |> avoidStruts . maximizeWithPadding 0 . smartBorders
        fullscreenBorderless =
            noBorders Full
        tiled =
            Tall nmaster delta mwfact
        nmaster =
            1
        delta =
            3 / 100
        mwfact =
            2 / 5

-- }}}



-- {{{ START UP / SHUTDOWN

myStartupHook :: X ()
myStartupHook =
    let
        startupCommands =
            [ "udevil clean"
            , "mount ~/.passwords/"
            -- Services
            , "urxvtd -f -q"
            , "pasystray"
            , "workrave"
            , "~/.bin/mailcheck.sh"
            , "picom -b -cf"
            , "hkhue redshift -g 1"
            , "unclutter -idle 2"
            , "hs-notifications"
            , "keepassxc ~/.passwords/pavans_passwords.kdbx"
            , "nvidia-settings --load-config-only --config=/home/prikhi/.nvidia-settings-rc"
            -- Apps
            , "firefox"
            , "signal-desktop --start-in-tray"
            ]
    in
        StatusBar.startupHook
            >> mapM_ spawnOnce startupCommands

myShutdownHook :: X ()
myShutdownHook =
    spawn "pkill keepassxc"
        >> StatusBar.stopHook

-- }}}



-- {{{ LOGGING

-- | Log the Workspace & Window status for the xmobars.
myLogHook :: X ()
myLogHook =
    xmobarLogHook
    <+> transparencyLogHook
    <+> spawn "xdotool-all hs-notifications windowraise"


-- | Fade Out All Unfocused Windows, With Exceptions For Programs in
-- `ignoreTransparencyClasses`.
transparencyLogHook :: X ()
transparencyLogHook =
    fadeOutLogHook fade
    where
        inactiveTransparency :: Rational
        inactiveTransparency =
            0.975
        activeTransparency :: Rational
        activeTransparency =
            0.985
        fade :: Query Rational
        fade = do
            unfocused <- isUnfocused
            ignored <- isIgnored
            return $
                if ignored then
                    1
                else if unfocused then
                    inactiveTransparency
                else
                    activeTransparency
        isIgnored :: Query Bool
        isIgnored =
            foldl (<||>) (return False) opaqueChecks
        opaqueChecks :: [Query Bool]
        opaqueChecks =
            map (className =?) ignoreTransparencyClasses


-- | Render each Screen's Workspaces into their own xmobar, highlighting
-- the current Screen's window title.
--
-- TODO: Move to `StatusBar` module?
xmobarLogHook :: X ()
xmobarLogHook =
    multiPPFormat
        (onlyCurrentScreen >=> withCurrentIcon >=> dynamicLogString)
        Theme.focusedScreenPP
        Theme.unfocusedScreenPP
    where
        -- Only show workspaces on the bar's screen
        onlyCurrentScreen :: PP -> X PP
        onlyCurrentScreen pp =
            hideOffScreen pp <$> gets (windowset .> W.current .> W.screen)
        -- Hide any workspaces on other screens
        hideOffScreen :: PP -> ScreenId -> PP
        hideOffScreen pp screenId =
            pp { ppHidden = showIfPrefix screenId True
               , ppHiddenNoWindows = showIfPrefix screenId False
               , ppUrgent = showUrgent screenId
               }
        -- Only show the workspace if it's prefix matches the current screen.
        --
        -- Note: Will throw a `read: no parse` error if the WorkspaceId has
        -- already had the screen prefix removed.
        showIfPrefix :: ScreenId -> Bool -> WorkspaceId -> String
        showIfPrefix screenId hasWindows workspaceId =
            if screenId == unmarshallS workspaceId then
                unmarshallW workspaceId |> \n ->
                    if hasWindows then
                        Theme.icon Theme.HiddenWorkspaceHasWindows ++ n ++ " "
                    else
                        pad n
            else
                ""
        -- Show urgent workspaces on the current screen with an icon and
        -- special background color.
        showUrgent :: ScreenId -> WorkspaceId -> String
        showUrgent screenId workspaceId =
            let prefixRemoved = drop 1 $ showIfPrefix screenId False workspaceId
            in
            if prefixRemoved == "" then
                ""
            else
                Theme.icon Theme.UrgentWorkspaceHasWindows
                    ++ Theme.urgentWorkspace prefixRemoved
        -- Add an icon to visible workspaces with windows
        withCurrentIcon :: PP -> X PP
        withCurrentIcon pp = do
            hasWindows <- gets $
                windowset
                    .> W.current
                    .> W.workspace
                    .> W.stack
                    .> W.integrate'
                    .> (not . null)
            return $ pp { ppCurrent = renderCurrentWorkspace hasWindows }
        -- Render the workspace name
        renderCurrentWorkspace :: Bool -> String -> String
        renderCurrentWorkspace hasWindows name =
            unmarshallW name |> \n -> Theme.currentWorkspace $
                if hasWindows then
                    Theme.icon Theme.CurrentWorkspaceHasWindows ++ n ++ " "
                else
                    pad n

-- }}}



-- {{{ WORKSPACES

myWorkspaces :: ScreenId -> [PhysicalWorkspace]
myWorkspaces =
    flip withScreens
        [ "term"
        , "www"
        , "code"
        , "chat"
        , "draw"
        , "media"
        , "proj"
        , "learn"
        , "misc"
        ]

-- }}}



-- {{{ CLIENT MANAGEMENT

floatingClasses :: [String]
floatingClasses =
    [ "pinentry"
    , "keepassx"
    , "Steam"
    , "steam"
    , "steamwebhelper"
    , "Lutris"
    , "battle.net.exe"
    , "heroic"
    , "VirtualBox"
    , "Edmarketconnector"
    ]
    ++ graphicsClasses
    ++ chatClasses
    ++ officeClasses
    ++ vmClasses

ignoreTransparencyClasses :: [String]
ignoreTransparencyClasses = concat
    [ graphicsClasses
    , officeClasses
    , mediaClasses
    , vmClasses
    , gameClasses
    ]

graphicsClasses :: [String]
graphicsClasses =
    [ "Gimp"
    , "Inkscape"
    , "Pencil"
    ]

chatClasses :: [String]
chatClasses =
    [ "Mumble"
    , "Pidgin"
    ]

officeClasses :: [String]
officeClasses =
    [ "libreoffice"
    , "libreoffice-calc"
    , "libreoffice-draw"
    , "libreoffice-startcenter"
    , "libreoffice-writer"
    , "soffice"
    ]

mediaClasses :: [String]
mediaClasses =
    [ "MComix"
    , "Mirage"
    , "Vlc"
    , "Zathura"
    , "feh"
    , "mpv"
    ]

vmClasses :: [String]
vmClasses =
    [ "VirtualBox"
    , "virt-manager"
    , "Genymotion"
    , "player"
    ]

gameClasses :: [String]
gameClasses =
    [ "Civ5XP"
    , "DefendersQuest"
    , "Dolphin-emu"
    , "FrozenSynapse"
    , "KOTOR2"
    , "Mainwindow.py"   -- PlayOnLinux
    , "Mojosetup"       -- GOG Installer
    , "Pcsx2"
    , "Psychonauts"
    , "RogueCastle.bin.x86_64"
    , "Steam"
    , "steam"
    , "steamwebhelper"
    , "battle.net.exe"
    , "Torchlight2.bin.x86_64"
    , "Wine"
    , "X3TC_main"
    , "csgo_linux"
    , "dota_linux"
    , "eu4"
    , "hoi4"
    , "ns2_linux32"
    , "stellaris"
    , "superhexagon.x86_64"
    ]

myManageHook :: ManageHook
myManageHook = composeAll <|
    [ isNotification --> doIgnore
    , className =? "Pale moon" --> shiftAndView 0 "www"
    , className =? "Firefox" --> shiftAndView 0 "www"
    , className =? "newmoon" --> shiftAndView 1 "www"
    , className =? "Chromium" --> shiftAndView 2 "www"
    ]
    ++ map (\name -> className =? name --> doFloat) floatingClasses
    ++ map (\name -> className =? name --> shiftAndView 0 "draw") graphicsClasses
    ++ map (\name -> className =? name --> shiftAndView 0 "chat") chatClasses
    ++ map (\name -> className =? name --> shiftAndView 0 "office") officeClasses
    ++ map (\name -> className =? name --> shiftAndView 0 "media") mediaClasses
    ++ map (\name -> className =? name --> shiftAndView 0 "misc") vmClasses
    where
        isNotification :: Query Bool
        isNotification =
            isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_NOTIFICATION"
        shiftAndView :: ScreenId -> WorkspaceId -> ManageHook
        shiftAndView s t =
            doF $ viewOnScreen s (marshall  s t) . W.shift (marshall s t)

-- }}}



-- {{{ EVENTS

-- | Respawn Status Bars When Multi-Head Configuration Changes.
myEventHook :: Event -> X All
myEventHook =
    StatusBar.eventHook

-- }}}



-- {{{ KEYBINDINGS

type KeyMap = Map.Map (KeyMask, KeySym) (X ())

myKeys :: XConfig l -> KeyMap
myKeys c@XConfig { modMask = modm } = Map.fromList $
    [
    -- MPD

    -- Play / Pause
      ( ( modm .|. shiftMask, xK_p )
      , spawn "mpc toggle"
      )
    -- Next / Previous Track
    , ( ( modm .|. shiftMask, xK_comma )
      , spawn "mpc prev"
      )
    , ( ( modm .|. shiftMask, xK_period )
      , spawn "mpc next"
      )

    -- VOLUME
    , ( ( noModMask, 0x1008ff11 )
      , spawn "volume-control.sh down"
      )
    , ( ( noModMask, 0x1008ff12 )
      , spawn "volume-control.sh toggle-mute"
      )
    , ( ( noModMask, 0x1008ff13 )
      , spawn "volume-control.sh up"
      )


    -- MISC UTILITIES

    -- Take a Screenshot
    , ( ( noModMask, xK_Print )
      , spawn "scrot"
      )
    -- Take a Screenshot & Open it in Gimp
    , ( ( shiftMask, xK_Print )
      , spawn "scrot -e 'gimp $f'"
      )
    -- Screenshot a Specific Windows by Adding the Control Modifier
    , ( ( controlMask, xK_Print )
      , spawn "scrot -s"
      )
    , ( ( shiftMask .|. controlMask, xK_Print )
      , spawn "scrot -s -e 'gimp $f'"
      )

    -- Toggle Between Headset & Speakers
    , ( ( modm, xK_F9)
      , spawn "toggle-audio.sh"
      )

    -- Toggle Redshift On & Off
    , ( ( modm, xK_F10 )
      , spawn "toggle-redshift.sh"
      )

    -- Toggle Workrave On & Off
    , ( ( modm, xK_F11 )
      , spawn "pkill workrave || workrave"
      )
    -- Toggle Picom On & Off
    , ( ( modm, xK_F12 )
      , spawn "pkill picom || picom -b -cf"
      )

    -- Toggle Floating
    , ( ( modm .|. controlMask, xK_z )
      , withFocused $ \windowId -> do
            floatingWindows <- gets (W.floating . windowset)
            if windowId `Map.member` floatingWindows then
                windows $ W.sink windowId
            else
                float windowId
      )

    -- Lock Screen
    , ( ( modm .|. controlMask .|. shiftMask, xK_Escape )
      , spawn "dm-tool lock"
      )



    -- EXTENSIONS



    -- Shell Prompt
    , ( ( modm, xK_r )
      , shellPrompt promptConfig
      )

    -- Maximize
    , ( ( modm, xK_m )
      , withFocused (sendMessage . maximizeRestore)
      )

    -- Cycle Through Workspaces
    , ( ( modm, xK_Right )
      , withWindowSet $
            W.current
                .> W.screen
                .> onScreen
                .> return
                .> WSIs
                .> moveTo Next
    )
    , ( ( modm, xK_Left )
      , withWindowSet $
            W.current
                .> W.screen
                .> onScreen
                .> return
                .> WSIs
                .> moveTo Prev
      )

    -- Cycle Through Screens
    , ( ( modm .|. controlMask, xK_j )
      , nextScreen >> moveCursorToFocus
      )
    , ( ( modm .|. controlMask, xK_k )
      , prevScreen >> moveCursorToFocus
      )

    -- Move Through Screens
    , ( ( modm, xK_i )
      , shiftNextScreen >> nextScreen >> moveCursorToFocus
    )
    , ( ( modm, xK_o )
      , shiftPrevScreen >> prevScreen >> moveCursorToFocus
    )

    -- Jump to Previous Workspace on Screen
    , ( ( modm, xK_semicolon )
      , withWindowSet $ \ss ->
          W.workspaces ss
            |> filter (onScreen (W.current ss |> W.screen) .> not)
            |> map W.tag
            |> toggleWS'
      )

    -- Directional Window Navigation
    , ( ( modm, xK_h )
      , windowGo L False
      )
    , ( ( modm, xK_j )
      , windowGo D False
      )
    , ( ( modm, xK_k )
      , windowGo U False
      )
    , ( ( modm, xK_l )
      , windowGo R False
      )

    -- Directional Window Swaps
    , ( ( modm .|. shiftMask, xK_h )
      , windowSwap L False
      )
    , ( ( modm .|. shiftMask, xK_j )
      , windowSwap D False
      )
    , ( ( modm .|. shiftMask, xK_k )
      , windowSwap U False
      )
    , ( ( modm .|. shiftMask, xK_l )
      , windowSwap R False
      )


    -- GENERAL


    -- Run Terminal on <M-Enter>
    , ( ( modm, xK_Return )
      , spawn <| terminal c
      )

    -- Kill Focused Window
    , ( ( modm .|. shiftMask, xK_c)
      , kill
      )

    -- Next / First Layout
    , ( ( modm, xK_z )
      , sendMessage NextLayout
      )
    , ( ( modm .|. shiftMask, xK_z )
      , sendMessage FirstLayout
      )

    -- Change Focus
    , ( ( modm, xK_s )
      , windows W.focusDown
      )
    , ( ( modm, xK_d )
      , windows W.focusUp
      )
    , ( ( modm, xK_y )
      , windows W.focusMaster
      )

    -- Move Clients
    , ( ( modm .|. shiftMask, xK_j )
      , windows W.swapDown
      )
    , ( ( modm .|. shiftMask, xK_k )
      , windows W.swapUp
      )
    , ( ( modm, xK_BackSpace )
      , windows W.swapMaster
      )

    -- Resize Master Area
    , ( ( modm .|. shiftMask, xK_h )
      , sendMessage Shrink
      )
    , ( ( modm .|. shiftMask, xK_l )
      , sendMessage Expand
      )

    -- Modify Master Window Count
    , ( ( modm .|. shiftMask, xK_u )
      , sendMessage <| IncMasterN 1
      )
    , ( ( modm .|. shiftMask, xK_i )
      , sendMessage <| IncMasterN (-1)
      )

    -- Quit / Restart
    , ( ( modm .|. shiftMask, xK_q )
      , myShutdownHook >> io exitSuccess
      )
    , ( ( modm, xK_q )
      , spawn "xmonad --recompile"
          >> StatusBar.stopHook
          >> spawn "xmonad --restart"
      )

    ]
    ++
    -- Switch / Move to Workspace
    [ ( ( modm .|. mask, key )
      , windows <| onCurrentScreen action index
      )
    | (index, key) <- zip (workspaces' c) [xK_1 .. xK_9]
    , (action, mask) <- [ ( W.greedyView, 0 ), ( W.shift, shiftMask ) ]
    ]

-- }}}



-- {{{ PROMPT CONFIGURATION

promptConfig :: XPConfig
promptConfig = def
    { maxComplRows =
        Just 10
    , promptBorderWidth =
        0
    , position =
        Top
    , font =
        Theme.xftFont
    , fgColor =
        Theme.promptForeground
    , bgColor =
        Theme.promptBackground
    , fgHLight =
        Theme.promptForegroundHighlight
    , bgHLight =
        Theme.promptBackgroundHighlight
    }

-- }}}



-- {{{ UTILS

-- | Determine if the `WindowSpace` is on the supplied `Screen`.
--
-- Assumes that the tag has been prefixed by the IndependentScreens
-- extension to contain `1_` where `1` is the tag's screen number.
onScreen :: ScreenId -> WindowSpace -> Bool
onScreen (S screenId) ws =
    (show screenId ++ "_") `isPrefixOf` W.tag ws

-- | Move the Cursor to Approximately the Center of the Focused Window
moveCursorToFocus :: X ()
moveCursorToFocus =
    updatePointer (0.5, 0.65) (0.25, 0.25)

-- }}}
