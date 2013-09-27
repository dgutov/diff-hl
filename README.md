About
=====

`diff-hl-mode` highlights uncommitted changes on the left side of the window,
allows you to jump between and revert them selectively.

For the usage instructions and the list of commands, see the Commentary section
inside the file.

Tested with Git, Mercurial, Bazaar and SVN. May work with other VC backends, too.

The package also contains auxiliary modes:

* `diff-hl-dired-mode` provides similar functionality in Dired.
* `diff-hl-margin-mode` changes the highlighting function to
  use the margin instead of the fringe.
* `diff-hl-amend-mode` shifts the reference revision back by one.

Check out the Commentary section in each respective file for the usage
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

Emacs 24+. On OS X, Emacs 24.3 or higher is recommended.

Notes
=====

* Since it uses the corresponding VC diff command, it's only accurate when the
  buffer is in saved state. Highlighting changes "on the fly" might be better,
  maybe we can do something similar to `highlight-markup-buffers` with a hidden
  buffer containing the unmodified copy.

* We conflict with other modes when they put indicators on the fringe,
  such as [Flycheck](https://github.com/flycheck/flycheck). This is
  rarely a significant problem, since if you're using such a mode,
  you'd usually want to fix all errors and warnings before continuing,
  and then the conflicting indicators go away.

* There's no fringe when Emacs is running in the console, but the navigation and
  revert commands still work. Consider turning `diff-hl-margin-mode` on.

* Frame-local and buffer-local values of `line-spacing` are not supported.

* [git-gutter](https://github.com/syohex/emacs-git-gutter) provides interactive
  commands to show/hide/toggle margin indicators for the same information, and
  allows you to customize how the indicators look.

Integration
=====

If you're using some package other than `vc` to commit changes, it might
not run `vc-checkin-hook` after commits. In that case, you'll need to
either add `diff-hl-update` to the hook it does run, or advise some
function that's called in the buffer after its state has changed.

psvn
-----

```lisp
(defadvice svn-status-update-modeline (after svn-update-diff-hl activate)
  (diff-hl-update))
```
