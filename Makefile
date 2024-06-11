.PHONY: all
all:
	cabal build

.PHONY: clean
clean:
	cabal clean

PAGER?= less
.PHONY: lint
lint:
	hlint src --color=always -j | ${PAGER}
