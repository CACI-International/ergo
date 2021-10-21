# Local Install
One may run ergo from a distribution by:
- running directly from the unpacked distribution, or
- creating a forwarding script or symlink to call ergo in the unpacked
  distribution (and putting that file in a `PATH` folder).

# User Install
One may install ergo for the user by copying the ergo binary to a user `PATH`
folder, and copying the ergo share folder (`share/ergo`) in the distribution to
the XDG program local data directory:
- **Linux**: `$XDG_DATA_HOME/ergo` or `~/.local/share/ergo`
- **Mac**: `~/Library/Application Support/ergo`
- **Windows**: `{FOLDERID_LocalAppData}/ergo/data`

# System Install
One may install ergo for the system by copying the ergo binary into a `bin`
folder (the folder must have the name `bin`), and copying the ergo `share/ergo`
folder into a `share` folder that is a sibling of the `bin` folder.

# MacOS-specific directions
On Mac, you may find that the ergo standard library is quarantined if you
downloaded it from the internet and have GateKeeper and quarantining enabled.
This will cause ergo to be killed when it tries to load the library. You can
remove the quarantine with
```
xattr -dr com.apple.quarantine path/to/ergo/share/folder
```
