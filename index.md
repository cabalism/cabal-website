---
title: The Haskell Cabal
---

Cabal: Common Architecture for Building Applications and Libraries

<div class="bs-callout bs-callout-info">
  <p>
    <h4>Cabal and Stack: which one should I install?</h4>
    We recommend installing both. Most Haskell projects can be built using Cabal, but some might require Stack. Installing both guarantees that you can use either, and while following a tutorial or book you can use whatever they recommend.
  </p>
</div>

## Quick Links

- [Hackage: the Haskell Package Database][hackage]
- [Browse open issues or report a bug][issues]

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

## History

Proposals for the library infrastructure that became Cabal.

- [Draft Version 1.0](/proposal-1.0/)
- [Draft Version 1.1](/proposal-1.1/)
- [Final Proposal](/proposal/)

## Posts
$partial("templates/post-list.html")$

…or you can find more in the [archives](/blog).

[hackage]: http://hackage.haskell.org/
[issues]: https://github.com/haskell/cabal/issues?utf8=%E2%9C%93&q=is%3Aopen
