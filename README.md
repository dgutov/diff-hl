About
=====

`diff-hl-mode` provides IDE-like highlighting of `vc-diff` results on the left
fringe of the current buffer.

Since it uses the corresponding VC diff command, it's only accurate when the
buffer is in saved state.

Additionally, it defines and binds

* `diff-hl-diff-goto-hunk` (`C-x v =`)
* `diff-hl-revert-hunk` (`C-x v n`)
* `diff-hl-previous-hunk` (`C-x v [`)
* `diff-hl-next-hunk` (`C-x v ]`)

Tested with Git, Mercurial, and Bazaar. May work with other VC backends, too.

Usage
=====

Ensure `diff-hl.el` is in a directory on your load-path, and add the following
to your `~/.emacs` or `~/.emacs.d/init.el`

    (require 'diff-hl)

and, to use it in all buffers

    (global-diff-hl-mode)

or, for example, to use it in all `prog-mode` and `vc-dir-mode` buffers

    (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
    (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)
