module Main where

import XMonad (xmonad)

import Config (myConfig)


main :: IO ()
main =
    myConfig >>= xmonad
