About
=====

`diff-hl-mode` provides IDE-like highlighting on the fringe to the left of the
changed buffer lines, compared to current HEAD.

Since it uses the corresponding VC diff command, it's only accurate when a
buffer is in saved state.

Tested with Git, Mercurial, and Bazaar. May work with other VC backends, too.

Usage
=====

Ensure diff-hl.el is in a directory on your load-path, and add the following to
your `~/.emacs` or `~/.emacs.d/init.el`

    (require 'diff-hl)

and, to use it in all buffers

    (global-diff-hl-mode)

or, for example, to use it in all `prog-mode` buffers

    (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
