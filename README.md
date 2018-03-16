# Pavan's Potential xmonad Configuration.

I really like awesome but would love to write configs in haskell instead of
lua.


## Usage

* Clone this repo to somewhere like `~/.config/xmonad/`.
* In `~/.xprofile` set `XDG_CONFIG_DIR` to this folder.
* Log out & in to reload `.xprofile` & run `xmonad --recompile`.

While developing, you can run `stack build --file-watch`.


## TODO:

Things Awesome does that I like:

* Focus Follows Mouse, Mouse Follows Focus
* Independent Workspaces per Monitor
* Directional move & shift, better switch/move to monitor bindings, cycle tags, etc.
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
* Click window to minimize
* Minimize / Maximize windows
* Per-Monitor status bars
* Increase / Decrease Column Count
* Jump to Urgent
* Run Prompt
* ncmpcpp dropdown
* Toggling workrave / compton
* Autostart apps(runonce)
* Keep on Top keybinding
* Swap w/ Master

Extra things I want it to do:

* Proper dynamic tagging + autotagging new windows
  (sometimes assigned tags get fucked in awesome)
* Per monitor available or default layouts
  (vertical stack portrait monitor by default)
* Workspace-specific positioning on init
    * If first load of gimp, put main in center & toolbars to left right side,
      all floating.
* Build w/ Stack


## License

GPL-3.0
