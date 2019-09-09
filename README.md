[![builds.sr.ht status](https://builds.sr.ht/~pvsr/mpd-status.svg)](https://builds.sr.ht/~pvsr/mpd-status?)
# `mpd-status`
`mpd-status` is an interactive block for
[`i3blocks`](https://github.com/vivien/i3blocks).
It shows a brief description of the current song (e.g. "{Artist} - {Song}
[paused | {vol}%]") and does various actions when clicked.
By default, left click plays and pauses, right click stops, the scroll wheel
changes the volume, middle click shuffles all albums, and, if your mouse has
them, the back button returns to the previous song and the forward button skips
to the next album (this can be changed to go to the next song by editing
`I3blocks/ButtonMap.hs` and changing `buttonToOp = albumShuffleButtons` to
`buttonToOp = defaultButtons`).

### Cool things about `mpd-status`
* Automatically updates when the song or volume change, whether the change is
	triggered by a click action, an external command like `mpc`, or just the
	current song ending. Most existing blocks are `mpc`-based scripts which
	can't easily do this
* MPD actions can be customized using
	[`libmpd-haskell`](https://github.com/vimus/libmpd-haskell)'s DSL
	* E.g. `clear >> add "" >> random True >> play Nothing` replaces the current
		queue with your library and shuffles it
	* This also allows complex actions like shuffling by album and skipping the
		beginning of the next album
* Written in Haskell (ymmv)

### Not so cool things
* It's a multi-file binary, not a script. Technically you could interpret it
	with `runhaskell`, but that's noticeably slow in my experience

### Planned features
* Customizable status format
* Support for MPRIS players
* Support for `mpv` (Maybe. It has an IPC server, but it's not especially
	suitable for this)

# Installation
At some point I'll start distributing binaries,
but for now you'll need `cabal-install`.
```bash
$ git clone https://github.com/pvsr/mpd-status
$ cd mpd-status
$ cabal new-update
$ cabal new-install mpd-status
```
Then add `~/.cabal/bin/` to your `PATH`.
