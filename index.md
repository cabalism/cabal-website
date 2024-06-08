---
title: The Haskell Cabal
---

## Quick Links

- [How to install a Cabal package][installing]
- [Hackage: the Haskell Package Database][hackage]
- [How to make a Cabal package][how-to]
- [2.0 migration guide][migration]
- [API][cabal-api]
- [Browse open issues or report a bug][issues]
- Questions can be [sent][mailto-libraries] to the [Haskell libraries mailing list][libraries-list]

## Comon Architecture for Building Applications and Libraries

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

## Posts
$partial("templates/post-list.html")$

…or you can find more in the [archives][/blog].

[installing]: http://cabal.readthedocs.io/en/latest/installing-packages.html#building-and-installing-packages
[hackage]: http://hackage.haskell.org/
[how-to]: https://wiki.haskell.org/How_to_write_a_Haskell_program
[migration]: https://github.com/haskell/cabal/wiki/2.0-migration-guide
[cabal-api]: https://www.haskell.org/cabal/release/cabal-latest/doc/API/Cabal/
[issues]: https://github.com/haskell/cabal/issues?utf8=%E2%9C%93&q=is%3Aopen
[mailto-libraries]: mailto:libraries@haskell.org
[libraries-list]: http://www.haskell.org/mailman/listinfo/libraries
