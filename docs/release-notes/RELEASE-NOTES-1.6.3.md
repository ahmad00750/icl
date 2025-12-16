# ICL 1.6.3 Release Notes

## Bug Fixes

### Fixed output from libraries that cache streams at initialization
- Libraries like `llog` that capture `*standard-output*` at logger creation time now display their output correctly
- Previously, ICL redirected streams during evaluation to a temporary string buffer, which broke libraries that cached stream references
- Output now flows to the inferior Lisp's stdout, which a background reader thread displays in real-time

## Technical Changes

- Added background output reader thread that continuously reads from the inferior Lisp process
- Removed stream redirection wrapper in `slynk-eval-form`
- Output from background threads in the inferior Lisp is now visible

## Breaking Changes

None.
