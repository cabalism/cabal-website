# Cabal Website

The http://www.haskell.org/cabal/ website.

## Local Development

> [!NOTE]
> The `cabal-website` Haskell package has one executable named `site`.

Build the project and then watch from the `develop` or another development
branch.  If you are not making any style changes then, with
[pnpm](https://pnpm.io/), you will need to run the front end build script once
after installing :

```
$ cat package.json
{
  "name": "cabal-website",
  ...
  "scripts": {
    ...
    "build": "cabal run site -- clean && cabal run site -- build && pnpm run js-copy && pnpm run style && cabal run site -- build",
    "watch": "cabal run site -- watch"
  }
}
```

1. **Installing**

    ```
    $ pnpm install
    Lockfile is up to date, resolution step is skipped
    Packages: +65
    +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    Progress: resolved 65, reused 65, downloaded 0, added 65, done

    dependencies:
    + @fortawesome/fontawesome-free 6.5.2
    + @popperjs/core 2.11.8
    + automated-a11y-sass 0.0.0 <- automated-a11y-sass
    + bootstrap 5.3.3

    devDependencies:
    + @biomejs/biome 1.8.1
    + purgecss 6.0.0
    + sass 1.77.4
    ```

2. **Building**

    ```
    $ pnpm run build

    > cabal-website@ build /.../cabal-website
    > cabal run site -- clean && cabal run site -- build && pnpm run js-copy && pnpm run style && cabal run site -- build

    Removing _site...
    Removing _cache...
    Removing _cache/tmp...
    Initialising...
    Creating store...
    Creating provider...
    Running rules...
    Checking for out-of-date items
    Compiling
    updated templates/post.html
    updated css/app.css
    updated css/app.css.map
    ...
    updated index.md
    updated pages/download.md
    updated pages/faq.md
    updated pages/history.md
    updated blog/index.html
    Success

    > cabal-website@ js-copy /home/philderbeast/dev/src/cabalism/cabal-website
    > pnpm run js-copy-dir && pnpm run js-copy-bootstrap && pnpm run js-copy-popper

    > cabal-website@ js-copy-dir /home/philderbeast/dev/src/cabalism/cabal-website
    > mkdir -p js

    > cabal-website@ js-copy-bootstrap /home/philderbeast/dev/src/cabalism/cabal-website
    > cp node_modules/bootstrap/dist/js/bootstrap.min.* js/

    > cabal-website@ js-copy-popper /home/philderbeast/dev/src/cabalism/cabal-website
    > cp node_modules/@popperjs/core/dist/umd/popper.min.js js/ && cp node_modules/@popperjs/core/dist/umd/popper.min.js.map js/

    > cabal-website@ style /home/philderbeast/dev/src/cabalism/cabal-website
    > pnpm run sass-to-css-working && pnpm run purge-css && pnpm run format-css && cp .css-working/as-formatted.css css/app.css

    > cabal-website@ sass-to-css-working /home/philderbeast/dev/src/cabalism/cabal-website
    > mkdir -p .css-working && sass app.sass:.css-working/app.css --load-path ./node_modules

    > cabal-website@ purge-css /home/philderbeast/dev/src/cabalism/cabal-website
    > purgecss --config ./purge-css-static.config.js --css .css-working/app.css --output .css-working/as-purged.css

    > cabal-website@ format-css /home/philderbeast/dev/src/cabalism/cabal-website
    > pnpm run format-copy && pnpm run format-write

    > cabal-website@ format-copy /home/philderbeast/dev/src/cabalism/cabal-website
    > cp .css-working/as-purged.css .css-working/as-formatted.css

    > cabal-website@ format-write /home/philderbeast/dev/src/cabalism/cabal-website
    > biome format .css-working/as-formatted.css --write --no-errors-on-unmatched

    Initialising...
    Creating store...
    Creating provider...
    Running rules...
    Checking for out-of-date items
    Compiling
    updated css/app.css
    updated js/bootstrap.min.js
    updated js/bootstrap.min.js.map
    updated js/popper.min.js
    updated js/popper.min.js.map
    Success
    ```

3. **Watching**

    ```
    $ pnpm run watch

    > cabal-website@ watch /.../cabal-website
    > cabal run site -- watch

    Listening on http://127.0.0.1:8000
    Initialising...
    Creating store...
    Creating provider...
    Running rules...
    Checking for out-of-date items
    Compiling
    Using async runtime with 1 threads...
    Success
    ```

### Without CSS or JS Changes

You can continue with `pnpm run watch` or use `cabal` to do watching:

```
$ cabal run site -- watch
Listening on http://127.0.0.1:8000
Initialising...
  Creating store...
  Creating provider...
  Running rules...
Checking for out-of-date items
Compiling
Success
```

## Release Process

Assuming development takes place on a `develop` branch (TODO: Rename `master` to
`develop`) and pages are published to a `gh/pages` branch, then clean, build and
deploy by copying the contents of `_site` to the root:

```
$ git checkout develop
... (make changes and commit on develop)
$ pnpm run build
$ git checkout gh/pages
$ cp -a _site/. .
... (commit changes brought over from develop via copy from _site onto gh/pages)
$ git push
```
