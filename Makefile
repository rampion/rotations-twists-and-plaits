default:
	cabal build Braid

all: install-dependencies configure build test

configure:
	cabal configure --enable-tests -f development

install-dependencies:
	cabal install --enable-tests --dependencies-only

build:
	cabal build

test:
	cabal test --show-details=always
