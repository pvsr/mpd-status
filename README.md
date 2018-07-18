# mpd-status

I use this with i3blocks to show and easily manipulate MPD's status. It started off as a pretty simple bash script that wrapped mpc and then  rewrote that in haskell with [turtle](https://github.com/Gabriel439/Haskell-Turtle-Library) and then I discovered [libmpd-haskell](https://github.com/vimus/libmpd-haskell) and lost control.

### Pros
* Automatically updates when the song changes!
* Complex MPD operations can be expressed simply
	* E.g. `clear >> add "" >> random True >> play Nothing` replaces the queue with your library and shuffles it
* Haskell
### Cons
* Requires my fork of i3blocks for click events to do anything
* Haskell
	* Most i3blocks scripts are just that—scripts—and are easy to download and run. This isn't so much
	* So many dependencies
