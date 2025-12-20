# ICL 1.9.2 Release Notes

## Bug Fixes

### Browser Assets
- Fix browser assets (CSS/JS) not loading in installed packages (RPM/DEB). The assets directory path was computed at build time instead of runtime, causing 404 errors for dockview.css, xterm.css, etc.

## Breaking Changes

None.
