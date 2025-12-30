# ICL 1.19.4 Release Notes

## Summary

Bug fix release with improved error handling, dead code cleanup, and new packaging features.

## Bug Fixes

- **Fix `,dis` command hanging on undefined symbols**: The `,dis` command now properly handles undefined symbols and non-function symbols by returning an error message instead of hanging indefinitely.

- **Fix `query-terminal-background` redefinition warning**: Removed duplicate function definition that caused a warning during compilation.

- **Fix `icl-tests.asd` syntax error**: Corrected misplaced closing parenthesis that broke `make check`.

## New Features

- **Bash completion for CLI options**: Tab completion now works for all ICL command-line options when installed via deb/rpm packages. Includes smart completion for `--lisp` (implementation names), `--load` (Lisp files), and `--connect` (host:port suggestions).

- **Man page**: Comprehensive man page (`man icl`) documenting all CLI options, REPL commands, keyboard shortcuts, configuration files, and usage examples.

## Code Quality

- **Dead code removal**: Removed ~340 lines of unused code across multiple files, improving maintainability and reducing binary size slightly.

## Installation

The bash completion and man page are automatically installed when using the deb or rpm packages:
- Bash completion: `/usr/share/bash-completion/completions/icl`
- Man page: `/usr/share/man/man1/icl.1`
