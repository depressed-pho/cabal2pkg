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

# ----------------------------------------------------------------------
all: README.md

README.md: doc/cabal2pkg.8
	{ \
		echo '<!--'; \
		echo 'DO NOT EDIT: This file is generated with the following command:'; \
		echo '  pandoc --from mdoc --to gfm $>'; \
		echo '-->'; \
	} | pandoc --from mdoc --to gfm --include-in-header=/dev/stdin $> \
		> $@.tmp
	mv -f $@.tmp $@ # Overwrite $@ only if the above command succeeded.
