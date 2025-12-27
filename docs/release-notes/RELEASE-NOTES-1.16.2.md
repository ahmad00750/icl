# ICL 1.16.2 Release Notes

## New Features

- Added `icl-runtime:+version+` to track which ICL version injected the runtime
  - Version is dynamically injected from the baked-in ICL version at image dump time

## Bug Fixes

- Fixed browser visualization not refreshing on Emacs editor interactions
  - Now hooks `interactive-eval` (C-x C-e), `compile-string-for-emacs` (C-c C-c),
    `eval-and-grab-output`, and `pprint-eval` in addition to REPL eval
  - Updated both standalone ICL and Emacs integration (icl.el)

- Fixed stale cached SBCL images not being cleared on ICL rebuild
  - Makefile now clears `~/.cache/icl/*` before building new image

## Breaking Changes

None.
