CABAL_VERSION ?= e7657ad319b9a5b50718262488d17aad915aad90
CABAL_URL := https://github.com/haskell/cabal/archive/${CABAL_VERSION}.tar.gz

all: .cabal-archive/cabal-${CABAL_VERSION}/README.md

.cabal-archive/cabal-${CABAL_VERSION}/README.md:
	rm -rf release-notes
	curl -sSL ${CABAL_URL} | tar -xz
	mkdir -p .cabal-archive
	mv cabal-*/release-notes -T release-notes
	mv cabal-*/ -t .cabal-archive