module Config where


import XMonad
import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog

import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Hooks.ManageDocks (docks, avoidStruts)
import XMonad.Layout.IndependentScreens (countScreens, withScreens, onCurrentScreen, workspaces')
import XMonad.Prompt (XPConfig(..), XPPosition(..))
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Util.Run (spawnPipe)

import Data.List (isPrefixOf)
import Data.Monoid (Endo)
import Flow
import System.Exit (exitSuccess)
import System.IO (Handle, hPutStrLn)

import qualified Data.Map as Map
import qualified XMonad.StackSet as W

import qualified Theme


-- {{{ CONFIGURATION

-- | Super simple for now, will extend w/ AwesomeWM features I like.
--
-- Starts a xmobar on the primary monitor & feeds it the status bar text.
myConfig = do
    -- TODO: Loop screen count, make multiple bars using `-x` option.
    statusBarHandle <- spawnPipe "xmobar"
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
            layoutHook def |> myLayoutHook
        , logHook =
            myLogHook statusBarHandle
        , workspaces =
            myWorkspaces screenCount
        , manageHook =
            myManageHook <+> manageHook def
        , keys =
            myKeys
        }
        |> docks
        |> (return :: a -> IO a)

-- }}}



-- {{{ LAYOUTS

-- | Prevent windows from overlapping status bar.
myLayoutHook =
    avoidStruts

-- }}}



-- {{{ LOGGING

-- | Output the workspace & window information to `xmobar`.
myLogHook :: Handle -> X ()
myLogHook statusBarHandle =
    dynamicLogWithPP xmobarPP
        { ppOutput = hPutStrLn statusBarHandle
        , ppTitle = xmobarColor Theme.green "" . shorten 100
        }

-- }}}



-- {{{ WORKSPACES

myWorkspaces =
    flip withScreens
        [ "term"
        , "www"
        , "code"
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



-- {{{ KEYBINDINGS

type KeyMap = Map.Map (KeyMask, KeySym) (X ())

myKeys :: XConfig l -> KeyMap
myKeys c@XConfig { modMask = modm } = Map.fromList $
    [

    -- EXTENSIONS

    -- Shell Prompt
      ( ( modm, xK_x )
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
      , shiftPrevScreen >> prevScreen >> moveCursorToFocus
    )
    , ( ( modm, xK_o )
      , shiftNextScreen >> nextScreen >> moveCursorToFocus
    )

    -- Jump to Previous Workspace on Screen
    , ( ( modm, xK_semicolon )
      , withWindowSet $ \ss ->
          W.workspaces ss
            |> filter (onScreen (W.current ss |> W.screen) .> not)
            |> map W.tag
            |> toggleWS'
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
    , ( ( modm, xK_j )
      , windows W.focusDown
      )
    , ( ( modm, xK_k )
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
      , io <| exitSuccess
      )
    , ( ( modm, xK_q )
      , spawn "xmonad --recompile && xmonad --restart"
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
    { promptBorderWidth =
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
