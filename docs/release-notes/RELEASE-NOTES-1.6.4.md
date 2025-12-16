# ICL 1.6.4 Release Notes

## New Features

### AI Integration
- Added `,explain` command for AI-powered explanations of code, errors, and results
- MCP server for AI tool integration - gives AI read-only access to the live Lisp environment
- Support for Gemini CLI, Claude CLI, and Codex CLI
- Streaming AI output with incremental markdown rendering
- Visual tool call markers and elapsed time display

### Packaging
- Added Chocolatey.org publishing support for Windows users

## Bug Fixes

- Fixed explain result quoting and inspector cleanup
- Suppressed Slynk loading warnings and startup messages
- Reduced ocicl verbosity during system loading

## Breaking Changes

None.
