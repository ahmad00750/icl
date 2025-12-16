# ICL 1.6.5 Release Notes

## New Features

### History Search
- **Alt+P**: History search backward - find entries starting with current text (prefix match)
- **Alt+N**: History search forward - navigate through prefix matches
- Case-insensitive matching

### Reverse Search Improvements
- Full multi-line display in reverse-i-search (Ctrl+R) - shows complete matched entry
- Display updates in place without scrolling artifacts
- Fixed double-render when accepting search result

### AI Integration
- Added MCP server support for Claude CLI

### Terminal Improvements
- Bracketed paste mode support for proper multi-line paste handling

## Bug Fixes

- Fixed reverse-i-search display clearing when accepting a match

## Dependencies

- Updated tuition dependency

## Breaking Changes

None.
