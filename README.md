# Cabal Website

The http://www.haskell.org/cabal/ website.

## Local Development

> [!NOTE]
> To use `cabal` instead of `stack`, substitute `cabal` for `stack` in the code
snippets.

Build the project and then watch from the `develop` or another development branch:

1. **Build** the `cabal-website` package that has one executable named `site`:
    ```
    $ stack build
    Building all executables for cabal-website once.
    ...
    cabal-website> configure (exe)
    Configuring cabal-website-0.1.0.0...
    cabal-website> build (exe)
    Preprocessing executable 'site' for cabal-website-0.1.0.0..
    Building executable 'site' for cabal-website-0.1.0.0..
    [1 of 1] Compiling Main
    [2 of 2] Linking .stack-work/dist/x86_64-linux/ghc-9.4.7/build/site/site
    cabal-website> copy/register
    Installing executable site in /.../cabal-website/.stack-work/.../bin
    ```

2. **Watch** and open a web browser page that the site is served on locally
(http://127.0.0.1:8000 is reported in the example) after which you can see edits
to pages and posts by refreshing the web browser:
    ```
    $ stack exec site watch
    Listening on http://127.0.0.1:8000
    Initialising...
      Creating store...
      Creating provider...
      Running rules...
    Checking for out-of-date items
    Compiling
    ...
    Success
    ```

## Release Process

Assuming development takes place on a `develop` branch (TODO: Rename `master` to
`develop`) and pages are published to a `gh/pages` branch, then clean, build and
deploy:

1. Cleaning
    ```
    $ stack exec site clean
    Removing _site...
    Removing _cache...
    Removing _cache/tmp...
    ```

2. Building

    ```
    $ stack exec site build
    Initialising...
      Creating store...
      Creating provider...
      Running rules...
    Checking for out-of-date items
    Compiling
    ...
    Success
    ```

3.  Deploying
    ```
    $ git checkout develop
    ... (make changes and commit on develop)
    $ stack exec site clean
    $ stack exec site build
    $ git checkout gh/pages
    $ cp -a _site/. .
    ... (commit changes brought over from develop via copy from _site onto gh/pages)
    $ git push
    ```