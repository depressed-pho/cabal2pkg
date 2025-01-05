# Revision history for cabal2pkg

## 0.1.0.2 -- not released yet

* Leave a TODO mark on `CATEGORIES` if it's `wip`.

## 0.1.0.1 -- 2025-01-05

* Better generation of `COMMENT`: now the leading "a" or "an" is dropped,
  the first word is title-cased, and the trailing period is removed.
* Unpackaged dependencies in `Makefile` are now clearly indicated as so.
* Fix an issue with `DESCR` where a monospaced word (e.g. `@foo@`) could be
  sometimes concatenated with surrounding words without spaces in between.
* Fix an issue where packages from tag-based GitHub releases could
  have wrong `WRKSRC` in some cases.

## 0.1.0.0 -- 2025-01-05

* First version. Released on an unsuspecting world.