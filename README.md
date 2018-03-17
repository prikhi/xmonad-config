# Pavan's Potential xmonad Configuration.

I really like awesome but would love to write configs in haskell instead of
lua.


## Usage

* Clone this repo to somewhere like `~/.config/xmonad/`.
* In `~/.xprofile` set `XMONAD_CONFIG_DIR` to this folder.
* Log out & in to reload `.xprofile` & run `xmonad --recompile`.

While developing, you can run `stack build --file-watch`.


## TODO:

Things Awesome does that I like:

* Directional move & shift, etc.
* Keyboard shortcut popup
* On click tag selection
* Multiple tag viewing, Clients on multiple tags
* Keybindings to/from floating
* Floating layout
* Shutdown commands
* Notifications + Shortcut to Clear
* Layout icons
* Active / Inactive Tag Colors, "Has Windows" Icons
* MPD, CPU, Net, Systray, Clock Widget
* List all windows on current tag
* Click window in taskbar to minimize
* Minimize / Maximize windows
* Increase / Decrease Column Count
* Jump to Urgent
* ncmpcpp dropdown  (Util.Scratchpad)
* Toggling workrave / compton
* Autostart apps(runonce)
* Keep on Top keybinding
* Per-Client Opacity Settings, Ignore Transparency Classes, Unfocused Dim

Extra things I want it to do:

* Quick note taking to a file   (Prompt.AppendFile)
* Generate xmobar config in the xmonad config, write it to file on start(in
  cache folder), launch xmobar w/ generated file. Would let me config the rest
  of the status bar in my theme. Could have a "short" xmobar config for narrow
  screens.
* Proper dynamic tagging + autotagging new windows
  (sometimes assigned tags get fucked in awesome)
* Jump to window prompt w/ autocomplete
* Workspace-specific positioning on init
    * Gimp: Put image in center & toolbars to left & right side, all floating.
    * Audio: non-mixer & jamin stacked on left, Claudia on right
* Project-specific workspaces that open terminals and run commands
    * SESE Website: Open main term w/ server/client vim & file tabs, another
      term w/ files & server repl tabs, & third term with build script, move
      palemoon & chromium to appropriate screens.


Links to do some of that stuff:

* https://hackage.haskell.org/package/xmonad-contrib-0.13/docs/XMonad-Hooks-Minimize.html
* https://hackage.haskell.org/package/xmonad-contrib-0.13/docs/XMonad-Layout-Maximize.html
* https://hackage.haskell.org/package/xmonad-contrib-0.13/docs/XMonad-Layout-Minimize.html
* https://hackage.haskell.org/package/xmonad-contrib-0.13/docs/XMonad-Layout-MultiColumns.html
* https://stackoverflow.com/questions/22838932/how-can-xmonads-dynamiclog-format-the-titles-of-unfocused-windows

## License

GPL-3.0
