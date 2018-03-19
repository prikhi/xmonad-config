module Config where


import XMonad
import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog

import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Actions.Navigation2D (withNavigation2DConfig, windowGo, windowSwap, Direction2D(..))
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.InsertPosition (insertPosition, Focus(Newer), Position(End))
import XMonad.Hooks.ManageDocks (docks, avoidStruts)
import XMonad.Hooks.DynamicBars (multiPPFormat)
import XMonad.Layout.IndependentScreens (countScreens, withScreens, onCurrentScreen, workspaces')
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.PerScreen (ifWider)
import XMonad.Prompt (XPConfig(..), XPPosition(..))
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Util.SpawnOnce (spawnOnce)

import Data.List (isPrefixOf)
import Data.Monoid (All, Endo)
import Flow
import System.Exit (ExitCode(..), exitSuccess)
import System.Process (readProcessWithExitCode)

import qualified Data.Map as Map
import qualified XMonad.StackSet as W

import qualified Theme
import qualified StatusBar


-- {{{ CONFIGURATION

-- | Super simple for now, will extend w/ AwesomeWM features I like.
--
-- Starts a xmobar on the primary monitor & feeds it the status bar text.
myConfig = do
    screenCount <- countScreens
    def { terminal =
            "urxvt"
        , modMask =
            mod4Mask
        , focusFollowsMouse =
            True
        , normalBorderColor =
            Theme.background
        , focusedBorderColor =
            Theme.orange
        , layoutHook =
            myLayoutHook
        , startupHook =
            myStartupHook
        , logHook =
            myLogHook
        , workspaces =
            myWorkspaces screenCount
        , manageHook =
            insertPosition End Newer <+> myManageHook
        , handleEventHook =
            myEventHook <+> handleEventHook def
        , keys =
            myKeys
        }
        |> withNavigation2DConfig def
        |> fixMPVFullscreen
        |> ewmh
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
                |> avoidStruts . smartBorders
        fullscreenBorderless =
            noBorders Full
        tiled =
            Tall nmaster delta mwfact
        nmaster =
            1
        delta =
            3 / 100
        mwfact =
            2 / 3

-- }}}



-- {{{ START UP

myStartupHook =
    let
        startupCommands =
            [ "compton -b"
            , "keepassx -min -lock"
            , "mount ~/.passwords/"
            , "mumble"
            , "newmoon"
            , "palemoon"
            , "pasystray"
            , "pidgin"
            , "systemctl --user start redshift"
            , "udevil clean"
            , "unclutter -idle 2 -grab"
            , "urxvtd -f -o -q"
            , "workrave"
            , "~/.bin/mailcheck.sh"
            ]
    in
        StatusBar.startupHook
            >> mapM_ spawnOnce startupCommands

-- }}}



-- {{{ LOGGING

-- | Log the Workspace & Window status for the xmobars.
myLogHook :: X ()
myLogHook =
    xmobarLogHook

-- | Render each Screen's Workspaces into their own xmobar, highlighting
-- the current Screen's window title.
xmobarLogHook :: X ()
xmobarLogHook =
    multiPPFormat onlyCurrentScreen Theme.focusedScreenPP Theme.unfocusedScreenPP
    where
        -- Only show workspaces on the bar's screen
        onlyCurrentScreen :: PP -> X String
        onlyCurrentScreen pp =
            gets (windowset .> W.current .> W.screen)
                >>= hideOffScreen pp .> dynamicLogString
        -- Hide any hidden workspaces on other screens
        hideOffScreen :: PP -> ScreenId -> PP
        hideOffScreen pp (S screenId) =
            pp { ppHidden = showIfPrefix screenId
               , ppHiddenNoWindows = showIfPrefix screenId
               }
        -- Only show the workspace if it's prefix matches the current screen.
        showIfPrefix :: Int -> WorkspaceId -> String
        showIfPrefix screenId workspaceId =
            if (show screenId ++ "_") `isPrefixOf` workspaceId then
                workspaceId |> dropWhile (/= '_') |> drop 1 |> pad
            else
                ""

-- }}}



-- {{{ WORKSPACES

myWorkspaces =
    flip withScreens
        [ "term"
        , "www"
        , "code"
        , "chat"
        , "graphics"
        , "media"
        , "sese"
        , "fic"
        , "misc"
        ]

-- }}}



-- {{{ CLIENT MANAGEMENT

floatingClasses :: [String]
floatingClasses =
    [ "Gimp"
    , "pinentry"
    , "keepassx"
    , "pidgin"
    , "Mumble"
    , "Steam"
    , "VirtualBox"
    ]

myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll <|
    map (\name -> className =? name --> doFloat) floatingClasses

-- }}}



-- {{{ EVENTS

-- | Respond to Client Fullscreen Requests & Respawn Status Bars When
-- Multi-Head Configuration Changes.
myEventHook :: Event -> X All
myEventHook =
    fullscreenEventHook <+> StatusBar.eventHook

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


    -- MISC UTILITIES

    -- Toggle Workrave On & Off
    , ( ( modm, xK_F11 )
      , spawn "pkill workrave || workrave"
      )
    -- Toggle Compton On & Off
    , ( ( modm, xK_F12 )
      , spawn "pkill compton || compton -b"
      )



    -- EXTENSIONS



    -- Shell Prompt
    , ( ( modm, xK_r )
      , shellPrompt promptConfig
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
    , ( ( modm, xK_space )
      , sendMessage NextLayout
      )
    , ( ( modm .|. shiftMask, xK_space )
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

    -- Un-Float Window
    , ( ( modm .|. controlMask, xK_z )
      , withFocused <| windows <. W.sink
      )

    -- Quit / Restart
    , ( ( modm .|. shiftMask, xK_q )
      , StatusBar.stopHook >> io exitSuccess
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

-- | Fix the fact that XMonad doesn't set _NET_WM_STATE_FULLSCREEN, which
-- breaks mpv's `f` key fullscreen keybind.
fixMPVFullscreen :: XConfig l -> XConfig l
fixMPVFullscreen c = c
    { startupHook = startupHook c <+> setSupportedWithFullscreen
    }
    where
        setSupportedWithFullscreen :: X ()
        setSupportedWithFullscreen = withDisplay $ \dpy -> do
            r <- asks theRoot
            a <- getAtom "_NET_SUPPORTED"
            c <- getAtom "ATOM"
            supp <- mapM getAtom ["_NET_WM_STATE_HIDDEN"
                                ,"_NET_WM_STATE_FULLSCREEN"
                                ,"_NET_NUMBER_OF_DESKTOPS"
                                ,"_NET_CLIENT_LIST"
                                ,"_NET_CLIENT_LIST_STACKING"
                                ,"_NET_CURRENT_DESKTOP"
                                ,"_NET_DESKTOP_NAMES"
                                ,"_NET_ACTIVE_WINDOW"
                                ,"_NET_WM_DESKTOP"
                                ,"_NET_WM_STRUT"
                                ]
            io $ changeProperty32 dpy r a c propModeReplace (fmap fromIntegral supp)

-- }}}
