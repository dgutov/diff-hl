About
=====

`diff-hl-mode` highlights uncommitted changes on the left fringe of the window,
allows you to jump between and revert them selectively.

For the usage instructions and the list of commands, see the header comment.

Tested with Git, Mercurial, and Bazaar. May work with other VC backends, too.

Screenshot
=====

[![screenie](http://i.imgur.com/bC8dBs.png)](http://i.imgur.com/bC8dB.png)

Notes
=====

* Since it uses the corresponding VC diff command, it's only accurate when the
  buffer is in saved state. Highlighting changes "on the fly" might be better,
  maybe we can do something similar to `highlight-markup-buffers` with a hidden
  buffer containing the unmodified copy.

* [git-gutter](https://github.com/syohex/emacs-git-gutter) provides the commands
  to show/hide/toggle margin indicators for the same information, and allows you
  to customize how the indicators look.
