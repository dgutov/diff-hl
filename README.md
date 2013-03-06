About
=====

`diff-hl-mode` highlights uncommitted changes on the left fringe of the window,
allows you to jump between and revert them selectively.

For the usage instructions and the list of commands, see the header comment.

Tested with Git, Mercurial, Bazaar and SVN. May work with other VC backends, too.

Screenshot
=====

![screenie](screenshot.png)

Top window: buffer in this minor mode, bottom window: corresponding diff.

Requirements
=====

Emacs 24+. On OS X, Emacs 24.3 or higher is recommended.

Notes
=====

* Since it uses the corresponding VC diff command, it's only accurate when the
  buffer is in saved state. Highlighting changes "on the fly" might be better,
  maybe we can do something similar to `highlight-markup-buffers` with a hidden
  buffer containing the unmodified copy.

* There's no fringe when Emacs is running in the console, but the navigation and
  revert commands still work.

* [git-gutter](https://github.com/syohex/emacs-git-gutter) provides interactive
  commands to show/hide/toggle margin indicators for the same information, and
  allows you to customize how the indicators look.
