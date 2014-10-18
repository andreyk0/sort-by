PATH:=.cabal-sandbox/bin:$(PATH)
export PATH

default: build

%:
	cabal $@

init:
	cabal sandbox init

deps:
	cabal install --dependencies-only

clean:
	cabal clean


.PHONY: \
	clean \
	default \
	deps \
	init
