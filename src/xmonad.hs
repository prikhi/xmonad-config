module Main where


import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks (docks, avoidStruts)
import XMonad.Util.Run (spawnPipe)

import Data.Monoid (Endo)
import Flow
import System.IO (Handle, hPutStrLn)

import qualified Data.Map as Map


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
            myKeys <+> keys def
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

-- TODO: Migrate Awesome Keys, Look at Defaults & Remove Unwanted Ones
myKeys :: XConfig l -> KeyMap
myKeys c@XConfig { modMask = modm } = Map.fromList
  [ -- Run Terminal on <M-Enter>
    ( ( modm, xK_Return ), terminal c |> spawn )
  ]


-- }}}
