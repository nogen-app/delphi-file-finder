# delphi-file-finder

## About

Delphi-File-Finder (DFF)

Is a plugin for the Embarcadero Delphi IDE.

It adds a file search frame, that can search for .pas files used in the entire project.
Including the search paths, not just the files added to the project.

Using it is as simple as pressing the hotkey ctrl+T to open the window, and then search.
Do note the hotkey only works when viewing a pas file, and a project is active.

## Installation

### By downloading the release
1. Download the latest version here from github.
2. Run the exe file

### From source

1. Build the repo
2. Move the .dll file to a folder Delphi has access to
3. Add the .dll file to the delphi versions registered experts.
This is done by adding a string value to regit at.
Computer\HKEY_CURRENT_USER\Software\Embarcadero\BDS\23.0\Experts
The name dosent matter, just point it to the .dll file.

## Building

To build this project from source, you need to have [spring4d](https://bitbucket.org/sglienke/spring4d/src/master) installed. The project looks for them in the src\spring4d folder.
So build spring4d by following its instructions and moving the compiled files into the respective build folder.

Example for Win32, Debug
* src\spring43\Win32\Debug

After that its just a matter of opening the project for your delphi version and building.

## Roadmap

* Optimizing the search function by making use of SQLite FTS3/FTS4
* Improve the design of the frame
* Add settings menu to allow changing of the hotkey
* Add vim like bindings for navigating the search frame
* Get the package registered on gedit