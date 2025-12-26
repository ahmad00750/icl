# ICL 1.15.1 Release Notes

## Security Enhancements

### Visualization Security Hardening
- **HTML sanitization**: Custom `visualize` methods returning HTML are now sanitized server-side to remove scripts and event handlers
- **Mermaid strict mode**: Diagrams render with `securityLevel: 'strict'` to block click handlers
- **Vega sandboxing**: Expression functions disabled and AST-based evaluation enforced
- **Content Security Policy**: Browser interface now sends CSP headers blocking inline scripts
- **WebSocket origin validation**: Only accepts connections from localhost

These protections ensure that loading untrusted Lisp libraries with custom `visualize` methods cannot execute arbitrary JavaScript in your browser.

### Unsafe Mode Option
New `--unsafe-visualizations` CLI option disables security restrictions for trusted code that requires JavaScript in visualizations:

```bash
icl -b --unsafe-visualizations
```

When enabled:
- Mermaid uses 'loose' security level (allows click handlers)
- Vega allows custom expression functions
- HTML content bypasses server-side sanitization

## Bug Fixes

- Fixed crash when pressing up/down arrows with empty command history in browser mode
- Fixed escape sequence parsing in browser terminal (up arrow no longer shows `[A`)

## Other Changes

- Removed examples from `--help` output for cleaner display
