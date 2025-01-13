.PHONY: all
all:
	cabal build

.PHONY: clean
clean:
	cabal clean
	rm -rf .hie

PAGER?= less
.PHONY: lint
lint:
	cabal check | ${PAGER}
	hlint src --color=always -j | ${PAGER}

.PHONY: stan
stan:
	stan | ${PAGER} -r
