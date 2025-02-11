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
	@if which hlint >/dev/null 2>&1; then \
		cmd="hlint src --color=always -j | ${PAGER}"; \
		echo "$$cmd"; \
		eval "$$cmd"; \
	else \
		echo "hlint is not installed"; \
	fi

.PHONY: stan
stan:
	stan | ${PAGER} -r
