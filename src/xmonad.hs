module Main where


import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks (docks, avoidStruts)
import XMonad.Util.Run (spawnPipe)

import Data.Monoid (Endo)
import Flow
import System.Exit (exitSuccess)
import System.IO (Handle, hPutStrLn)

import qualified Data.Map as Map
import qualified XMonad.StackSet as W


main :: IO ()
main =
    myConfig >>= xmonad


-- {{{ CONFIGURATION

-- | Super simple for now, will extend w/ AwesomeWM features I like.
--
-- Starts a xmobar on the primary monitor & feeds it the status bar text.
myConfig = do
    -- TODO: Loop screen count, make multiple bars using `-x` option.
    statusBarHandle <- spawnPipe "xmobar"
    def { terminal =
            "urxvt"
        , modMask =
            mod4Mask
        , borderWidth =
            1
        , layoutHook =
            layoutHook def |> myLayoutHook
        , logHook =
            logHook def >> myLogHook statusBarHandle
        , manageHook =
            myManageHook
        , keys =
            myKeys
        }
        |> docks
        |> return

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
        , ppTitle = xmobarColor "#A6E22E" "" . shorten 50
        }

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

    -- Run Terminal on <M-Enter>
      ( ( modm, xK_Return )
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
    , ( ( modm .|. shiftMask .|. controlMask, xK_space )
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
      , windows <| action index
      )
    | (index, key) <- zip (workspaces c) [xK_1 .. xK_9]
    , (action, mask) <- [ ( W.greedyView, 0 ), ( W.shift, shiftMask ) ]
    ]
    ++
    -- Switch to Left / Center / Right Monitors
    [ ( ( modm .|. mask, key )
      , screenWorkspace screen >>= flip whenJust (action .> windows)
      )
    | (key, screen) <- zip [xK_e, xK_w, xK_r] [0 ..]    -- middle = screen 0
    , (action, mask) <- [ ( W.view, 0 ), ( W.shift, shiftMask ) ]
    ]


-- }}}
