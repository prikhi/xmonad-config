module Main where


import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks (docks, avoidStruts)
import XMonad.Util.Run (spawnPipe)
import System.IO (Handle, hPutStrLn)


main :: IO ()
main =
    xmonad =<< myConfig


-- | Super simple for now, will extend w/ AwesomeWM features I like.
--
-- Starts a xmobar on the primary monitor & feeds it the status bar text.
myConfig = do
    -- TODO: Loop screen count, make multiple bars using `-x` option.
    statusBarHandle <- spawnPipe "xmobar"
    return . docks $ def
        { terminal =
            "urxvt"
        , modMask =
            mod4Mask
        , borderWidth =
            1
        , layoutHook =
            myLayoutHook $ layoutHook def
        , logHook =
            logHook def >> myLogHook statusBarHandle
        }


-- | Prevent windows from overlapping status bar.
myLayoutHook =
    avoidStruts


-- | Output the workspace & window information to `xmobar`.
myLogHook :: Handle -> X ()
myLogHook statusBarHandle =
    dynamicLogWithPP xmobarPP
        { ppOutput = hPutStrLn statusBarHandle
        , ppTitle = xmobarColor "#A6E22E" "" . shorten 50
        }
