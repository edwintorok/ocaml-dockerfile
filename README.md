dockerfile —  Dockerfile eDSL and distribution support
-----------------------------------------------------
%%VERSION%%

[![OCaml-CI Build Status](https://img.shields.io/endpoint?url=https://ci.ocamllabs.io/badge/ocurrent/ocaml-dockerfile/master&logo=ocaml)](https://ci.ocamllabs.io/github/ocurrent/ocaml-dockerfile)

[Docker](https://docker.com) is a container manager that can build images
automatically by reading the instructions from a `Dockerfile`. A Dockerfile is
a text document that contains all the commands you would normally execute
manually in order to build a Docker image. By calling `docker build` from your
terminal, you can have Docker build your image step-by-step, executing the
instructions successively.  Read more at <https://docker.com>.

This library provides a typed OCaml interface to generating Dockerfiles
programmatically without having to resort to lots of shell scripting and
awk/sed-style assembly.

ocaml-dockerfile is distributed under the ISC license.

- **HTML Documentation**: <https://www.ocurrent.org/ocaml-dockerfile/>;
- **Source**: <https://github.com/ocurrent/ocaml-dockerfile>;
- **Issues**: <https://github.com/ocurrent/ocaml-dockerfile/issues>;
- **Email**: <anil@recoil.org>.

## Installation

dockerfile can be installed with `opam`:

    opam install dockerfile
    opam install dockerfile-opam
    opam install dockerfile-cmd

The `dockerfile-opam` package includes modules for OPAM- and Linux-specific
Dockerfile generation, such as common distributions.

If you don't use `opam` consult the [`opam`](dockerfile.opam) file for build
instructions.
