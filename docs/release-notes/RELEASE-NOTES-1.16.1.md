# ICL 1.16.1 Release Notes

## Bug Fixes

- Fixed terminal escape sequence race condition causing garbled output on startup
  - Added `--noinform` flag when starting cached SBCL image
  - Moved theme/background detection to before inferior Lisp startup
  - Terminal queries now happen while terminal is in clean state

## Breaking Changes

None.
