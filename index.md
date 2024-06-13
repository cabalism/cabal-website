---
title: Cabal
---

## Introduction

Cabal is an acronym of **C**ommon **A**rchitecture for **B**uilding
**A**pplications and **L**ibraries. Central to this architechure is `cabal`, the
executable (exe) build tool from the [cabal-install][cabal-install-pkg] package.

::: info
This architecture defines a common interface for package authors and
distributors to easily build their applications in a portable way. It is part of
a larger infrastructure for distributing, organizing, and cataloging Haskell
libraries and programs.  Specifically, it describes what a package is, how these
interact with the language, and what Haskell implementations (compilers) must to
do to support packages. It also specifies some infrastructure (code) that makes
it easy for tool authors to build and distribute conforming packages.
:::

::: warning
The architecture says nothing about more global issues such as how authors
decide where in the module name space their library should live; how users can
find a package they want; how orphan packages find new owners; and so on.
:::

## Packages

When developing anything non-trivial in Haskell, anything that larger than a
single `.hs` file, we pack modules (`.hs` files) into packages.

- **Module** - The unit of compilation in Haskell is the module. A module is a
  collection of related functions, types, and type classes. Modules are the
  primary unit of organization in Haskell programs.
- **Package** - The unit of distribution in Haskell is the package. A package is
  a collection of one or more modules, plus some metadata about the package
  (e.g., its name, version, and dependencies). Package components include
  libraries, executables, test suites and benchmarks.

Package source code can be published to Hackage and vetted by Stackage.

- **Hackage** - A central repository of Haskell packages. Hackage is a website
  that hosts packages and provides tools for searching, browsing, and
  downloading packages.
- **Stackage** - For each resolver Stackage pairs a GHC compiler version with a
  groups of packages from Hackage that build together. This group is pins each
  included package to an exact version.

  Stackage resolvers come in three sets; `nightly-yyyy-mm-dd` for the latest
  set, `lts-mm.nn` for stable packages and `ghc` for the very limited set of
  packages released with GHC. Each `lts-mm.*` series imposes some rules on
  included packages, such as not allowing breaking version bumps.

## Demo

Having installed `GHC` and `cabal` with GHCup on Ubuntu Linux, let's find `cabal`;

```shell
$$ type cabal
cabal is ~/.ghcup/bin/cabal

$$ readlink ~/.ghcup/bin/cabal
cabal-3.10.3.0

$$ type cabal-3.10.3.0
cabal-3.10.3.0 is ~/.ghcup/bin/cabal-3.10.3.0
```

We can use `cabal` to download its own package, `cabal-install`, from Hackage.

```shell
$$ VER=3.10.3.0 cabal get cabal-install-{$$VER}
Downloading  cabal-install-3.10.3.0
Downloaded   cabal-install-3.10.3.0
Unpacking to cabal-install-3.10.3.0/

$$ VER=3.10.3.0 cat cabal-install-{$$VER}/cabal-install.cabal
Cabal-Version:      2.2
Name:               cabal-install
Version:            3.10.3.0
...
executable cabal
...
```

From this source, we can install `cabal` it again, showing that `cabal` can
build and install local packages downloaded from Hackage.

```shell
$$ cd cabal-install-3.10.3.0/

$$ cabal install cabal-install:exe:cabal --overwrite-policy=always
Wrote tarball sdist to
/.../cabal-install-3.10.3.0/dist-newstyle/sdist/cabal-install-3.10.3.0.tar.gz
Resolving dependencies...
Build profile: -w ghc-9.8.2 -O1
In order, the following will be built (use -v for more details):
 - cabal-install-solver-3.10.3.0 (lib) (requires download & build)
 - hackage-security-0.6.2.6 (lib) (requires build)
 - cabal-install-3.10.3.0 (lib) (requires build)
 - cabal-install-3.10.3.0 (exe:cabal) (requires build)
Downloading  cabal-install-solver-3.10.3.0
Starting     hackage-security-0.6.2.6 (lib)
Downloaded   cabal-install-solver-3.10.3.0
Starting     cabal-install-solver-3.10.3.0 (lib)
Building     hackage-security-0.6.2.6 (lib)
Building     cabal-install-solver-3.10.3.0 (lib)
Installing   hackage-security-0.6.2.6 (lib)
Completed    hackage-security-0.6.2.6 (lib)
Installing   cabal-install-solver-3.10.3.0 (lib)
Completed    cabal-install-solver-3.10.3.0 (lib)
Starting     cabal-install-3.10.3.0 (lib)
Building     cabal-install-3.10.3.0 (lib)
Installing   cabal-install-3.10.3.0 (lib)
Completed    cabal-install-3.10.3.0 (lib)
Starting     cabal-install-3.10.3.0 (exe:cabal)
Building     cabal-install-3.10.3.0 (exe:cabal)
Installing   cabal-install-3.10.3.0 (exe:cabal)
Completed    cabal-install-3.10.3.0 (exe:cabal)
Symlinking 'cabal' to '~/.cabal/bin/cabal'
```

## Projects

Even larger, projects are a collections of packages. These allow us to develop a
set of related packages, to develop a product and to depend on unpublished
packages that we can get from source code repositories or other means.

- **Source Code Repositories** - It is possible to depend on packages that are
  in a source code repository.
- **Vendored Packages** - When source code for a package is copied locally and
  used if it was a local package.

## Build Tools

There are two main build tools in the Haskell ecosystem, Cabal and Stack. The
main difference between them is how they deal with dependencies in their
projects. Stack works with a Stackage resolver and any dependency that is not
included in the resolver must be pinned to an exact version as an extra
dependency. Cabal can work this way too but has a built-in dependency solver
that will pick versions of dependencies that are not pinned, if it can.




[cabal-install-pkg]: https://hackage.haskell.org/package/cabal-install