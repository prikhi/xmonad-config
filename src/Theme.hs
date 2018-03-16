module Theme where

import Data.List (intercalate)
import Flow


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
