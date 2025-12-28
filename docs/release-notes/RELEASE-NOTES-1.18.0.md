# ICL 1.18.0 Release Notes

## New Features

### Library Visualization Registration
Libraries can now provide custom visualizations that work even when loaded before ICL connects (e.g., when attaching ICL to a running Lisp from Emacs).

Define a `REGISTER-ICL-VIZ` function in your package:
```lisp
(defun register-icl-viz ()
  (defmethod icl-runtime:visualize ((obj my-type))
    (list :mermaid (my-to-mermaid obj))))
```

ICL automatically discovers and calls `REGISTER-ICL-VIZ` in all packages when `,viz` is invoked. Each package is processed only once per session.

### Package Exclusions for Visualization Registration
New configuration variable `*viz-package-exclusions*` allows filtering packages by regex patterns during visualization registration scanning.

Example in `~/.iclrc`:
```lisp
(setf icl:*viz-package-exclusions* '("^SB-" "^ASDF" "^SLYNK"))
```

### Browser Command Protection
The `,browser` command now detects when already running in browser mode and shows a message instead of attempting to start another browser session.

## Bug Fixes

- Fix undefined variable warnings during visualization registration
- Update examples to use the new `register-icl-viz` pattern (removing unnecessary `eval`)

## Breaking Changes

None.
