# ICL 1.1.1 Release Notes

## Summary

Bug fix release addressing color handling and terminal compatibility issues.

## Bug Fixes

- **Fixed NO_COLOR runtime detection** - The `NO_COLOR` environment variable is now checked at runtime, not just at compile time. This ensures ICL respects `NO_COLOR` regardless of how it was built.

- **Fixed terminal background detection** - Added `ICL_BACKGROUND` environment variable to override automatic terminal background detection. This fixes issues with terminals that don't support OSC 11 queries (like VHS for terminal recording).

## New Environment Variables

| Variable | Description |
|----------|-------------|
| `ICL_BACKGROUND` | Override terminal background detection (`dark` or `light`) |

## Installation

### Fedora/RHEL
```bash
sudo dnf install ./icl-1.1.1-1.*.x86_64.rpm
```

### Debian/Ubuntu
```bash
sudo apt install ./icl_1.1.1-1_amd64.deb
```

## Upgrading from 1.1.0

No breaking changes. Simply install the new version over the previous one.
