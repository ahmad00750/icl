# ICL 1.6.6 Release Notes

## Bug Fixes

### Package Handling
- Fixed prompt not updating when changing to packages that only exist in the inferior Lisp (e.g., after `,ql` loading a system)
- Fixed tab completion not working correctly in remote-only packages
- Fixed `,cd` command failing with "Package ICL does not exist" error for remote packages

### Inspector
- Fixed `,i` (inspect) not showing the correct last result when in a different package
- Fixed inspector leaving title bar artifact on screen when exiting

## Internal Changes

- Added `*icl-package-name*` variable to track remote package name separately from local package object
- Remote Lisp now updates `*`, `**`, `***` history variables after each evaluation

## Breaking Changes

None.
