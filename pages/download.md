---
title: Downloading Cabal
subtitle: Ways to download and install Cabal's executable tool <samp>cabal</samp>.
description: The tool's explicit target form is <samp>cabal-install:exe:cabal</samp>, the executable named <samp>cabal</samp> from the <samp>cabal-install</samp> package.
---

## Install/Upgrade

::: tip
Most people already have <samp>cabal</samp> because it is included with GHCup.
:::

If you want to upgrade to the latest stable version, use GHCup again or use `cabal` itself:

    $ cabal install cabal cabal-install

Sometimes the older installed version is still on the program search $PATH, you
can check you're running the latest version with:

    $ cabal --version

::: warning
If mixing installation methods, be careful about PATH precedence.
:::

## Cabal library (version 3.10.1.0)

[Released:]{style="font-weight: bolder"} March 2023\

[Source download:]{style="font-weight: bolder"}
[Cabal-3.10.1.0.tar.gz](https://downloads.haskell.org/~cabal/Cabal-3.10.1.0/Cabal-3.10.1.0.tar.gz)
(with a checksum in file SHA256SUMS in
[Cabal-3.10.1.0/](https://downloads.haskell.org/~cabal/Cabal-3.10.1.0/))

Please see the [User\'s guide](https://cabal.readthedocs.io), the [API
documentation](http://hackage.haskell.org/package/Cabal), and the
[change log](http://hackage.haskell.org/package/Cabal/changelog).

## cabal-install tool (version 3.10.1.0)

[cabal-install]{.inline-code} is the command line interface to Cabal and
hackage. This is the package that provides the [cabal]{.inline-code}
command line program. See the [change
log](http://hackage.haskell.org/package/cabal-install/changelog) for
information about what\'s new in this version.

[Released:]{style="font-weight: bolder"} March 2023\

[Source and binary downloads:]{style="font-weight: bolder"}
[cabal-install-3.10.1.0/](https://downloads.haskell.org/~cabal/cabal-install-3.10.1.0/)

Packages for Debian (multiple versions) are available on the
[Haskell.org APT repository](http://downloads.haskell.org/debian/).

Packages for Windows are available via
[Chocolatey](https://chocolatey.org/packages/cabal).

HEAD binaries for macOS are available on
[haskell.futurice.com](https://haskell.futurice.com/)

You also can use [ghcup](https://www.haskell.org/ghcup)

To build any of the source packages, you also need further packages,
which can be found on Hackage.

## Bugs

Report bugs [here](https://github.com/haskell/cabal/issues) or to the
[cabal-devel](mailto:cabal-devel@haskell.org) mailing list.

## Code

You can get the development version of the code [here](https://github.com/haskell/cabal).

## Version Numbers

Stable Releases are numbered `a.b.c.d`, where `b` is even. Unstable snapshots
between releases are numbered `a.b.c.d`, where `b` is odd. Changes for `c` and
`d` are minor.

For example, `2.1.x.x` is the development version leading up to the stable
release `2.2.x.x`.

## Older Releases

The versions bundled with recent Haskell implementation releases include:

-   GHC 9.6.1 includes Cabal 3.10.1.0
-   GHC 9.4.1 includes Cabal 3.8.1.0
-   GHC 9.2.4 includes Cabal 3.6.3.0
-   GHC 9.2.1 includes Cabal 3.6.0.0
-   GHC 9.0.2 includes Cabal 3.4.1.0
-   GHC 8.10.4 includes Cabal 3.2.1.0
-   GHC 8.8.4 includes Cabal 3.0.1.0
-   GHC 8.6.5 includes Cabal 2.4.0.1
-   GHC 8.4.4 includes Cabal 2.2.0.1
-   GHC 8.2.2 includes Cabal 2.0.1.0
-   GHC 8.0.2 includes Cabal 1.24.2.0
-   GHC 7.10.3 includes Cabal 1.22.5.0

You can browse older releases at [downloads.haskell.org/~cabal](https://downloads.haskell.org/~cabal).
