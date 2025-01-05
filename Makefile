.PHONY: all
all:
	cabal build

.PHONY: clean
clean:
	cabal clean

PAGER?= less
.PHONY: lint
lint:
	cabal check | ${PAGER}
	hlint src --color=always -j | ${PAGER}
