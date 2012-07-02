About
=====

`diff-hl-mode` provides IDE-like highlighting of `vc-diff` results on the left
fringe of the current buffer.

Since it uses the corresponding VC diff command, it's only accurate when the
buffer is in saved state.

Additionally, it defines and binds `diff-hl-diff-goto-hunk` and
`diff-hl-revert-hunk`, to `C-x v =` and `C-x v n` respectively.

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
