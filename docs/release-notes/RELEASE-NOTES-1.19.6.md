# ICL 1.19.6 Release Notes

## Summary

Bug fix release restoring ECL backend support and improving the `,dis` command.

## Bug Fixes

- **Fix ECL backend startup**: ECL (and potentially other Lisp implementations) failed to start because the boot script used `uiop:getcwd` directly. The UIOP package doesn't exist until ASDF loads, causing a reader error. Fixed by deferring the symbol lookup until runtime.

- **Fix `,dis` command to accept `#'function` syntax**: The disassemble command now accepts both symbol names (`,dis foo`) and function designators (`,dis #'foo`).

## Updates

- **Migrate xterm.js to scoped packages**: Updated from deprecated `xterm@5.3.0` to `@xterm/xterm@6.0.0` following the upstream package migration for security and maintenance.

## Breaking Changes

None.
