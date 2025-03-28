# Revision history for cabal2pkg

## 0.1.2 -- not released yet

Compatibility changes:

* Fixed build with GHC 9.10. `cabal2pkg` no longer supports GHC 9.8.

New features:

* Added a global option `--fill-column=COLUMN` for specifying page
  width. Currently only used for generating `DESCR` files. Previously it
  was fixed to 80, but now it's defaulted to 76. This options is only meant
  to be used for debugging.
* `cabal2pkg init` now infers a package from the name of the directory if
  no package URI is given. The package is assumed to be available in
  Hackage in this case.

Behaviour changes:

* `cabal2pkg` no longer hard-codes the path to GHC found at compile
  time. It now instead searches in PATH at run-time. This is because
  `cabal2pkg` is intended to be used while the pkgsrc tree is in an
  inconsistent state where it isn't always possible to rebuild `cabal2pkg`
  immediately after updating GHC.
* `cabal2pkg update` now temporarily removes
  `HASKELL_UNRESTRICT_DEPENDENCIES` before merging local changes. The
  variable is now completely overwritten rather than being patched during
  updates. This is because its value is not determined solely by the
  package itself but also by the surrounding environment (i.e. the versions
  of packages it dependson, including the ones that come along with GHC),
  and the environment might have changed since when the last time the
  package was updated. Attempting to merge changes to them thus tended to
  create merge conflicts that didn't really make sense.
* `cabal2pkg update` now tries to update `HASKELL_UNRESTRICT_DEPENDENCIES`
  even when the package is already the latest one.
* Newlines in `COMMENT` are now replaced with spaces.
* `cabal2pkg` now displays warnings about unknown dependencies.
* `cabal2pkg` now treats `custom-setup` as a Cabal package component,
  although it is technically not. Setup dependencies were previously
  ignored but are now build-dependend. (Yes, they are build-depended, not
  tool-depended because buildlink `BUILDLINK_DEPMETHOD` doesn't support
  tool dependencies. They have to be buildlinked, otherwise `Setup` cannot
  be linked.)
* Description of Manual Cabal flags are now parsed and rendered as a
  Haddock text on terminal.

Bug fixes:

* Fixed the `COMMIT_MSG` saying "no release notes have been provided"
  having no newlines at the end of file.
* Fixed an issue where `cabal2pkg update` aborts with an error for some
  packages on Hackage. It failed when the package to be updated had
  versions marked as `unpreferred` in its version list.
* Fixed a bug where `PKGNAME` could be incorrectly generated sometimes.
* Fixed a bug where `OPTPARSE_APPLICATIVE_EXECUTABLES` in some cases didn't
  get defined when it should.
* Fixed an issue where ordered, unordered, or definition lists in `DESCR`
  files were sometimes rendered incorrectly.
* Fixed a bug where `cabal2pkg update` could show a bogus message saying
  `Deleted buildlink3.mk` when there was no `buildlink3.mk` in the first
  place.
* Fixed an issue where package components (e.g. `lib:*`) with no visible
  dependencies could survive garbage collection in certain cases, leaving
  empty sections in Makefiles.
* Fixed an issue where Makefile conditionals could be rendered with wrong
  indentation.
* Cabal conditionals like `if !arch(x86_64)` are now translated to Makefile
  conditionals like `.if ${MACHINE_ARCH} != "x86_64"`. Previously they were
  translated to `.if !(${MACHINE_ARCH} == "x86_64")`, which were correct
  but weren't optimal.
* Fix an issue where `cabal2pkg` sometimes fails to simplify Makefile
  conditionals properly, leaving empty `.else` .. `.endif` blocks.
* Fixed an issue where a series of Cabal conditionals that are independent
  to each other could be wrongly translated to a single `.if` ... `.elif`
  ... `.endif` as opposed to a series of `.if` ... `.endif`.
* Fixed an issue where `USE_TOOLS+= pkg-config` would not be produced in
  certain cases even when there were pkg-config dependencies.
* Fixed an issue where `DESCR` files are rendered incorrectly in some
  cases.

## 0.1.1 -- 2025-01-11

* Fixed an issue where dependencies that are unacceptably old are listed in
  `HASKELL_UNRESTRICT_DEPENDENCIES`. This variable is supposed to only
  contain packages that are unacceptably new. Now `cabal2pkg` emits
  warnings when outdated dependencies are found.
* Fixed an issue where empty conflict markers could sometimes appear in
  updated files. This could happen when pkgsrc maintainers and the upstream
  made identical changes to the same portion of package metadata.
* `cabal2pkg` now deduplicates dependencies that appear in package metadata
  more than once.
* `cabal2pkg` now supports updating wip packages. It was previously
  unsupported for a technical reason but the limitation is now lifted.

## 0.1.0.4 -- 2025-01-09

* Fixed an issue where setting a command name (as opposed to a command
  path) to `MAKE` during build causes it to be searched from `PATH`.

## 0.1.0.3 -- 2025-01-05

* Fix an incorrect error message that is shown when updating a wip package
  is attempted. It is unsupported for a technical reason at the moment.

## 0.1.0.2 -- 2025-01-05

* Leave a TODO mark on `CATEGORIES` if it's `wip`.
* Fix an issue with `PACKAGE-URI` in the `init` subcommand where a bareword
  package name without version is misinterpreted as a package identifier
  with a null version.

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
