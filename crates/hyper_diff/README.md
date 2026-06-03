# HyperDiff

HyperDiff is an AST differencing library.
It reimplements the [Gumtree](https://hal.science/hal-01054552/document) algorithm in Rust,
using HyperAST as the underlying AST structure.
Other than integrating tree differencing algorithms to the HyperAST,
we optimized the algorithms to lazily decompress nodes,
enabling AST differencing and tracking on entire code-bases without ad hoc handling of files and directories.
