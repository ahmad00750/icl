# ICL 1.0.0 Release Notes

**Release Date:** December 2025

## Summary

Initial stable release of ICL (Interactive Common Lisp), an enhanced REPL for Common Lisp.

## Features

- **Multi-line input** with readline-style editing via linedit
- **Persistent command history** across sessions
- **Tab completion** for symbols and packages
- **Extensible comma-prefixed command system**
- **Support for multiple Lisp implementations** via Slynk:
  - SBCL, CCL, ECL, CLISP, ABCL, Clasp
- **Auto-detection** of available Lisp implementations
- **Connect to existing Slynk servers** with `--connect host:port`
- **Built-in commands:**
  - `,help` - Show available commands
  - `,doc` - Look up documentation
  - `,apropos` - Search for symbols
  - `,describe` - Describe objects
  - `,macroexpand` - Expand macros
  - `,trace` / `,untrace` - Trace function calls
  - `,inspect` - Inspect objects
  - `,cd` / `,pwd` - Change/print working directory
  - `,load` - Load Lisp files
  - `,quit` - Exit the REPL

## CLI Options

- `-e, --eval EXPR` - Evaluate expression and print result
- `-l, --load FILE` - Load a Lisp file before starting REPL
- `--lisp IMPL` - Specify Lisp implementation (sbcl, ccl, ecl, etc.)
- `--connect HOST:PORT` - Connect to existing Slynk server
- `--no-config` - Don't load ~/.iclrc
- `--no-banner` - Suppress startup banner

## Installation

### Fedora/RHEL
```bash
sudo dnf install ./icl-1.0.0-1.*.x86_64.rpm
```

### Debian/Ubuntu
```bash
sudo apt install ./icl_1.0.0-1_amd64.deb
```

## Known Limitations

- x86_64 Linux only (ARM64 and macOS packages planned)
- Requires a Lisp implementation (SBCL, CCL, etc.) in PATH
