# ICL 1.19.0 Release Notes

## New Features

### Monaco Source Viewer
The browser interface now uses Monaco (the engine behind VS Code) instead of highlight.js for all code viewing panels. This brings a rich viewing experience with:

- **Syntax Highlighting**: Full Common Lisp language support with proper tokenization for keywords, special variables, constants, strings, and comments
- **Hover Documentation**: Hover over any symbol to see its documentation and argument list via Slynk
- **Symbol Navigation**: Click any symbol to look it up in the Packages/Symbols/Symbol Info panels
- **Search**: Use Ctrl+F to search within any source panel
- **Copy to REPL**: Right-click context menu to send code to the REPL with automatic focus transfer
- **Theme Support**: Source panels automatically follow ICL's light/dark theme changes

### Multiple Definition Support for ,source
The `,source` command now shows all definitions for a symbol (e.g., function, compiler macro, setf expander) with a dropdown selector, instead of picking just one.

### Source Viewer Link in Symbol Info
The Symbol Info panel now includes a [Source] link alongside [Inspect] for functions, allowing quick navigation to source code.

### Remote File Support
The `,source` command now works with files on remote Slynk servers, displaying content in the Source panel when local files aren't accessible.

### Renamed Coverage Command
The `,cover-file` command has been renamed to `,cover-load` for consistency with `,cover-ql` (quickload).

## Bug Fixes

- Fix source panels not responding to theme changes
- Fix theme detection using darkP property from Lisp
- Fix AMD loader conflict by loading Monaco after Vega/Regulex
- Fix serving font files (.ttf, .woff, .woff2) for Monaco icons
- Fix code coverage to work with remote Slynk servers

## Breaking Changes

- `,cover-file` renamed to `,cover-load`
