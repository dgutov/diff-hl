About [![Build Status](https://github.com/dgutov/diff-hl/actions/workflows/ci.yml/badge.svg)](https://github.com/dgutov/diff-hl/actions/workflows/ci.yml)
=====

`diff-hl-mode` highlights uncommitted changes on the side of the window, allows
you to jump between and revert them selectively.

This feature is also known as "source control gutter indicators".

In buffers controlled by Git, you can stage and unstage the changes.

For the usage instructions and the list of commands, see the Commentary section
inside the file.

Tested with Git, Mercurial, Bazaar and SVN. May work with other VC backends, too.

The package also contains auxiliary modes:

* `diff-hl-dired-mode` provides similar functionality in Dired.
* `diff-hl-margin-mode` changes the highlighting function to
  use the margin instead of the fringe.
* But if you use a non-graphical terminal, the package will fall back to using
  the margins anyway, as long as `diff-hl-fallback-to-margin` is non-nil and the
  margin width is non-zero.
* `diff-hl-amend-mode` sets the reference revision to the one before
  recent one. Also, you could use `diff-hl-set-reference-rev` to set
  it to any revision, see its docstring for details.
* `diff-hl-flydiff-mode` implements highlighting changes on the fly.
* `diff-hl-show-hunk-mouse-mode` makes fringe and margin react to
  mouse clicks to show the corresponding hunk. That's the alternative
  to using `diff-hl-show-hunk` and friends.

Usage
=====

Put this into your init script:

```lisp
(global-diff-hl-mode)
```

You must also ensure that VC is enabled (e.g. `vc-handled-backends` is
not nil).

Check out the Commentary section in each file for more detailed usage
instructions.

Screenshots
=====

diff-hl-mode
-----
Top window: a buffer in this minor mode, bottom window: the corresponding diff.

![screenie](screenshot.png)

diff-hl-dired-mode
-----

![screenie](screenshot-dired.png)

diff-hl-margin-mode
-----

![screenie](screenshot-margin.png)

Requirements
=====

Emacs 26.1+.

Notes
=====

* By default `diff-hl-mode` only updates the display when the buffer is in
  saved state. For a different tradeoff, try `diff-hl-flydiff-mode`, which
  updates the buffer on a timer.

* To use an
  [alternative diff algorithm](http://stackoverflow.com/questions/32365271/whats-the-difference-between-git-diff-patience-and-git-diff-histogram)
  with Git, add a corresponding argument to `vc-git-diff-switches`,
  e.g. `(setq vc-git-diff-switches '("--histogram"))`. Using the
  `diff.algorithm` option doesn't work
  [because](http://article.gmane.org/gmane.comp.version-control.git/294622)
  `vc-git-diff` calls `git diff-index`. `diff-hl-flydiff-mode` does
  not support alternative algorithms, because it uses the external
  `diff` program.

* We conflict with other modes when they put indicators on the fringe,
  such as [Flycheck](https://github.com/flycheck/flycheck). This is
  rarely a significant problem, since if you're using such a mode,
  you'd usually want to fix all errors and warnings before continuing,
  and then the conflicting indicators go away.

* There's no fringe when Emacs is running in the console, but the navigation
  and revert commands still work. Consider turning `diff-hl-margin-mode` on,
  to show the indicators in the margin instead. It also helps avoid the conflict
  with Flycheck/Flymake even on graphical frames.

* Frame-local and buffer-local values of `line-spacing` are not supported.

* Fringe width up to 16 works best (because we can't define a bitmap
  with width above that number).

Integration
=====

If you're using some package other than `vc` to commit changes, it might
not run `vc-checkin-hook` after commits. In that case, you'll need to
either add `diff-hl-update` to the hook it does run, or advise some
function that's called in the buffer after its state has changed.

psvn
-----

```lisp
(advice-add 'svn-status-update-modeline :after #'diff-hl-update)
```

Magit
-----

If you're using a version before 2.4.0, it defines `magit-revert-buffer-hook`
(or `magit-not-reverted-hook`), which we use.

When using Magit 2.4 or newer, add this to your init script:

```lisp
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
```

Tramp
-----

`diff-hl` should just work with Tramp. But slow or high latency
connections can cause performance problems. If you experience such
issues, customize `diff-hl-disable-on-remote` to `t`. This will change
the behavior of both `turn-on-diff-hl-mode` and `global-diff-hl-mode`
(whichever you prefer to use).
