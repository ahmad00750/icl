# ICL 1.19.5 Release Notes

## Summary

Browser improvements including horizontal scrolling in the REPL terminal, updated JavaScript dependencies, and various bug fixes.

## New Features

- **Horizontal scrolling in browser REPL**: Long lines in the terminal no longer wrap. A horizontal scrollbar appears when content exceeds the panel width, making it easier to read wide output like tables and formatted data.

- **Visualization panel title prefix**: All visualization panels now display "viz: " prefix in their titles for easier identification in the panel list.

## Bug Fixes

- **Fix Monaco/regulex AMD loader conflict**: Resolved JavaScript error "undefined missing vs/editor/editor.main" by loading Monaco editor before regulex to prevent AMD loader conflicts.

- **Fix Monaco editor font loading**: Added `data:` to Content Security Policy font-src directive to allow Monaco's embedded codicon font to load properly.

- **Fix Dockview panel rendering**: Switched from ESM to UMD build of Dockview to fix panel initialization issues.

- **Fix ASDF system loading in backends**: Added current working directory to ASDF source registry, ensuring systems can be found when using remote Slynk connections.

## Updates

- **JavaScript dependencies updated**: Updated Dockview, xterm.js, Vega, Vega-Lite, Vega-Embed, and Mermaid to latest versions.

- **Vega-Lite examples updated**: Example files now use Vega-Lite v6 schema instead of deprecated v5.

## Breaking Changes

None.
