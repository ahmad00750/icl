# ICL 1.14.1 Release Notes

## New Features

### Mermaid Diagram Support
- Added Mermaid diagram rendering in the browser interface
- Automatic detection of Mermaid diagrams (flowchart, sequence, class, state, ER, Gantt, pie, journey, gitgraph, mindmap, timeline)
- Theme-aware rendering (dark/light mode support)
- Responsive to panel resizing
- Use `,viz` command with Mermaid definition strings

### Custom Visualization Enhancement
- Added `:mermaid` type for `icl-runtime:visualize` methods
- See `examples/mermaid.lisp` for a complete example

## Documentation
- Added `examples/mermaid.lisp` with flowchart, sequence diagram, class diagram, and state machine examples
- Updated README with Mermaid diagram support
