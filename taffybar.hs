module Main where

import Control.Monad.IO.Class (liftIO)
import Graphics.UI.Gtk (Widget)
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.SimpleConfig
import System.Taffybar.Widget.SNITray (sniTrayNew)
import System.Taffybar.Widget.SimpleClock
import System.Taffybar.Widget.Workspaces
import System.Taffybar.Widget.Weather

import qualified Theme

main :: IO ()
main =
  let
  in
    simpleTaffybar defaultSimpleTaffyConfig
      { barHeight = 17
      , startWidgets = [ workspaces ]
      , endWidgets = reverse [ weather, tray, clock ]
      }

workspaces :: TaffyIO Widget
workspaces =
    workspacesNew defaultWorkspacesConfig

weather :: TaffyIO Widget
weather =
    liftIO $ weatherNew weatherConfig 10
    where
        weatherConfig =
            (defaultWeatherConfig "K7W4")
                { weatherTemplate = Theme.weather "$tempF$ °F"
                }

tray :: TaffyIO Widget
tray =
    sniTrayNew

clock :: TaffyIO Widget
clock =
    textClockNew Nothing (Theme.date "%a %_d %b %H:%M") 1
