# Pavan's Potential xmonad Configuration.

I really like awesome but would love to write configs in haskell instead of
lua.


## Usage

You need `trayer` installed for the system tray to show up.

* Clone this repo to somewhere like `~/.config/xmonad/`.
* In `~/.profile` set `XMONAD_CONFIG_DIR` to this folder.
* Log out & in to reload `.profile` & run `xmonad --recompile && xmonad --restart`.

While developing, you can run `stack build --file-watch`.


## TODO:

Things Awesome does that I like:

* Keyboard shortcut popup
* On click tag selection
* Multiple tag viewing, Clients on multiple tags
* Keybindings to/from floating
* Floating layout
* Shutdown commands
* Notifications + Shortcut to Clear (see dunst)
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
* Keep on Top keybinding
* Per-Client Opacity Settings, Ignore Transparency Classes, Unfocused Dim
* Mod-RightMouseBtn to resize master width

Extra things I want it to do:

* Quick note taking to a file   (Prompt.AppendFile)
* Proper dynamic tagging + autotagging new windows
  (sometimes assigned tags get fucked in awesome)
    * Need to write custom XMonad.Prompt that will use DynamicWorkspaces
      extension to add new workspaces w/ names as expected by
      IndependentScreens extension. Not sure how to make dynamic keybindings to
      switch to newly created workspaces.
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
* https://hackage.haskell.org/package/xmonad-contrib-0.13/docs/XMonad-Util-ExtensibleState.html

## License

GPL-3.0
