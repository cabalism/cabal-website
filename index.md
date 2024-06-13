---
title: The Haskell Cabal
---

Cabal: Common Architecture for Building Applications and Libraries

## Common Architecture for Building Applications and Libraries

Cabal is a system for building and packaging Haskell libraries and programs. It
defines a common interface for package authors and distributors to easily build
their applications in a portable way. Cabal is part of a larger infrastructure
for distributing, organizing, and cataloging Haskell libraries and programs.

Specifically, the Cabal describes what a Haskell package is, how these packages
interact with the language, and what Haskell implementations must to do to
support packages. The Cabal also specifies some infrastructure (code) that makes
it easy for tool authors to build and distribute conforming packages.

The Cabal is only one contribution to the larger goal. In particular, the Cabal
says nothing about more global issues such as how authors decide where in the
module name space their library should live; how users can find a package they
want; how orphan packages find new owners; and so on.
