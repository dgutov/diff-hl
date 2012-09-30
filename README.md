About
=====

`diff-hl-mode` highlights uncommitted changes on the left fringe of the buffer
window, allows you to jump between them, and revert them selectively.

For the usage instructions and the list of commands, see the header comment.

Tested with Git, Mercurial, and Bazaar. May work with other VC backends, too.

Screenshot
=====

[![Foo](http://i.imgur.com/bC8dBs.png)](http://i.imgur.com/bC8dB.png)

Notes
=====

* Since it uses the corresponding VC diff command, it's only accurate when the
  buffer is in saved state. Highlighting changes "on the fly" might be better,
  maybe we can do something similar to `highlight-markup-buffers` with a hidden
  buffer containing the unmodified copy.

* Out of modes providing similar functionality, `highlight-changes-mode` is the
  closest I could find. Angry Fruit Salad aside, it may be fine for writing
  prose, but think it's pretty much useless for version-controlled files.
