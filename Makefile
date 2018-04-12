default:
	cabal build Braid

all: .cabal-sandbox install-dependencies configure build test

.cabal-sandbox:
	cabal sandbox init

configure:
	cabal configure --enable-tests -f development

install-dependencies:
	cabal install --enable-tests --dependencies-only

build:
	cabal build

test:
	cabal test doctest-readme --show-details=always

doc: README.html

README.html: README.md
	pandoc $< -o $@ --css gfm.css --standalone --from gfm --to html --metadata=title:README
