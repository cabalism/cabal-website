all: release-notes/cabal-install-3.10.3.0.md

CABAL_VERSION ?= bf6f26dc2c02cce24b0d0f429b1b0fdabaa14028
CABAL_URL := https://github.com/haskell/cabal/archive/${CABAL_VERSION}.tar.gz

release-notes/%.md:
	rm -rf cabal-release-notes
	curl -sSL ${CABAL_URL} | tar -xz
	mkdir -p .cabal-archive
	mv cabal-*/release-notes -T release-notes
	mv cabal-*/ -t .cabal-archive