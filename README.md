# `mpd-status`
This is a block for [`i3blocks`](https://github.com/vivien/i3blocks) that can show
and easily manipulate MPD's status.
`mpd-status`'s main distinguishing feature over similar blocks is that it can
both display song changes as they happen *and* control MPD's state.
Now that
[i3blocks#228](https://github.com/vivien/i3blocks/issues/228) is resolved,
that's not actually very difficult,
so others will probably have their own blocks that can do the same thing soon
enough,
but as far as I know this is the first!

### Cool things about mpd-status
* Automatically updates when the song changes!
* MPD actions can be expressed using
	[`libmpd-haskell`](https://github.com/vimus/libmpd-haskell)'s DSL
	* E.g. `clear >> add "" >> random True >> play Nothing` replaces the queue
		with your library and shuffles it
* Haskell
### Not so cool things
* Requires the latest master of i3blocks for now
* Haskell
	* Most i3blocks scripts are just that—scripts—and are easy to just download
		and run. That's not exactly a strong suit of Haskell's, partially thanks to:
	* So many dependencies
