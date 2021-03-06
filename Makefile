default: build test

all: .cabal-sandbox install-dependencies configure build test

.cabal-sandbox:
	cabal sandbox init

configure:
	cabal configure --enable-tests -f development

install-dependencies:
	cabal install --enable-tests --dependencies-only

build:
	cabal build rotations-twists-and-plaits

test: test-fast

test-fast: .cabal-sandbox
	.cabal-sandbox/bin/doctest --preserve-it -pgmL .cabal-sandbox/bin/markdown-unlit README.lhs

test-slow:
	cabal test doctest-readme --show-details=always

doc: README.html

README.html: README.md
	pandoc $< -o $@ --css gfm.css --standalone --from gfm --to html --metadata=title:README
