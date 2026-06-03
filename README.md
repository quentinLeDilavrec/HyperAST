# [HyperAST](https://hyperast.github.io/) &nbsp;&nbsp;&nbsp; [![Rust][Rust badge]][Rust badge] &nbsp;&nbsp; [![CICD badge]][CICD] &nbsp;&nbsp; [![DOI][zenodo-badge]][zenodo-link]
[CICD badge]:     https://github.com/HyperAST/HyperAST/actions/workflows/deploy.yml/badge.svg 
[CICD]:           https://github.com/HyperAST/HyperAST/actions/workflows/deploy.yml
[zenodo-badge]:   https://zenodo.org/badge/DOI/10.5281/zenodo.14810468.svg
[zenodo-link]:    https://doi.org/10.5281/zenodo.14810468
[zenodo-link]:    https://doi.org/10.5281/zenodo.14810468
[Rust badge]:     https://img.shields.io/badge/Rust-000000?style=for-the-badge&logo=rust&logoColor=white

<table class="tg"><thead><th>
  
| [Book](https://hyperast.github.io/book/index.html) | [GUI](https://hyperast.github.io/gui/index.html) | [Rust doc](https://hyperast.github.io/doc/hyperast/index.html)  |
|---|---|---|

#### [Getting Started](https://hyperast.github.io/book/quickstart/quickstart.html)

##### [Compute code Metrics(GUI)](https://hyperast.github.io/book/quickstart/compute_code_metrics.html)

##### [Track Code(GUI)](https://hyperast.github.io/book/quickstart/track_code.html)

</th><th><img src="./hyper_app/assets/icon-512.png" width="320px" /></th></thead></table>

## Summary

The HyperAST project aims to facilitate the exploration and processing of large source code histories.
At its core an HyperAST is a compressed syntax tree structure similar to the MerkleDAG of Git (but beyond files).
An HyperAST is typically constructed from a [Git](https://git-scm.com/) repository,
by parsing each unique file once with [TreeSitter](https://tree-sitter.github.io/tree-sitter/).
In the process, only structurally unique subtrees are stored and processed,
thus compressing and incrementally deriving useful information to be used later.

The project includes a use-def solver,
based on the context-free indexing of references present in subtrees (each subtree has a bloom filter of contained references).
It should be noted that this feature is currently being reworked to generalize beyond the analysis of Java projects.

It also includes a tree diff module, called HyperDiff, largely inspired by [Gumtree](https://github.com/GumTreeDiff/gumtree) and its [algorithms](https://hal.science/hal-04855170v1/document). We notably adapted the algorithms to lazily decompress nodes.

Many other features are provided, and can be discovered reading the [book](https://hyperast.github.io/book/index.html), the [rust doc](https://hyperast.github.io/doc/hyperast/index.html) or through the [graphical app](https://hyperast.github.io/gui/index.html).

## How to use 

You can use the dedicated [GUI](https://hyperast.github.io/gui/index.html) in your browser. However, in order to use any of the GUI features, you will need to launch/connect to the server. 

### Launch the server with [Cargo](https://github.com/rust-lang/cargo)
```sh
# from the project root dir, after having cloned the repository
cargo run -p backend --release 
```
> [!WARNING]
> You have to handle system dependencies yourself, such as, `rustc`, `openssl`.
> Please use [rustup](https://github.com/rust-lang/rustup) to automatically handle the build chain compatibility.

### Launch server with [Nix](https://nixos.org/)
```
nix run .#hyperast-backend
```
instead of `cargo run`, or without even cloning the repository
```
nix run github:HyperAST/HyperAST#hyperast-webapi # Nix handles everything!
```

Nix is a package manager for reproducible, declarative and reliable systems.
This will download all dependencies and build locally. 
It works easily on most *NIX systems (Linux, WSL, macOs, ...) and can produce OCI-images like Docker.

> [!TIP]
> Look [there](https://nixos.org/download) for instruction on how to install Nix on your system.

There is also a development shell provided with all the necessary dependencies installed in a healthy environment to develop and build the project.
You can enter the environment with:
```sh
nix develop # from the project root dir
```

## How to Cite

If you use HyperAST and HyperDiff for academic purposes, please cite the following papers:

```bibtex
@inproceedings{ledilavrec:hal-03764541,
  TITLE = {{HyperAST: Enabling Efficient Analysis of Software Histories at Scale}},
  AUTHOR = {Le Dilavrec, Quentin and Khelladi, Djamel Eddine and Blouin, Arnaud and J{\'e}z{\'e}quel, Jean-Marc},
  URL = {https://hal.inria.fr/hal-03764541},
  BOOKTITLE = {{ASE 2022 - 37th IEEE/ACM International Conference on Automated Software Engineering}},
  PUBLISHER = {{IEEE}},
  PAGES = {1-12},
  YEAR = {2022}
}
```

```bibtex
@inproceedings{ledilavrec:hal-04189855,
  TITLE = {{HyperDiff: Computing Source Code Diffs at Scale}},
  AUTHOR = {Le Dilavrec, Quentin and Khelladi, Djamel Eddine and Blouin, Arnaud and J{\'e}z{\'e}quel, Jean-Marc},
  URL = {https://inria.hal.science/hal-04189855},
  BOOKTITLE = {{ESEC/FSE 2023 - 31st ACM Joint European Software Engineering Conference and Symposium on the Foundations of Software Engineering}},
  PUBLISHER = {{ACM}},
  PAGES = {1-12},
  YEAR = {2023}
}
```
