<!--
DO NOT EDIT: This file is generated with the following command:
  pandoc --from mdoc --to gfm doc/cabal2pkg.8
-->
# NAME

`cabal2pkg` — automate importing/updating Haskell packages in pkgsrc

# SYNOPSIS

`cabal2pkg` \[option...\] `init` \[`-w`\| `--overwrite`\]
`PACKAGE-URI`  
`cabal2pkg` \[option...\] `update` \[`-f`\| `--force`\] \[`-m` `STYLE`\|
`--merge`=`STYLE`\] \[`PACKAGE-URI`\]

# DESCRIPTION

`cabal2pkg` is a tool to automate importing Haskell packages to pkgsrc
and updating existing Haskell packages in pkgsrc. `cabal2pkg` is similar
to <span class="Xr">url2pkg(8)</span>, in that it downloads a distfile,
extracts it, and generates package <span class="Pa">Makefile</span> and
other files, but it is specialised in Haskell packages and is intended
to generate mostly complete <span class="Pa">Makefile</span>.

`cabal2pkg` can also update existing packages by performing 3-way merge
on files. In the update mode, `cabal2pkg` fetches two package metadata
of the current and the updated version, then tries to apply differences
between two versions against the current set of files.

## Global options

The following options are available for all subcommands:

`--color`=`WHEN`, `--colour`=`WHEN`  
Use colours on output. `WHEN` can be `never`, `always`, or `auto`, where
`auto` enables colours if and only if the standard error is a terminal
(default: `auto`).

The environment variable `NO_COLOR` affects the default style of output.
See <span class="Sx">ENVIRONMENT</span> for details.

`-n`, `--no-commit-msg`  
Suppress creating a file named <span class="Pa">COMMIT_MSG</span> in the
package directory; (hopefully) suitable for committing changes to VCS.
`cabal2pkg` *never* makes commits automatically for you. It is your
responsibility to check if things are good to commit.

`-d`, `--debug`  
Show debugging output that is only useful for debugging `cabal2pkg`.

`-p` `DIR`, `--pkgdir`=`DIR`  
The path to the pkgsrc package directory to work with (default: `.`,
i.e. the current directory).

`-f` `FLAG`, `--flag`=`FLAG`  
Cabal package flags to apply, such as `+foo -bar`. This option can be
arbitrarily repeated. Using this option causes `CONFIGURE_ARGS` to
appear in <span class="Pa">Makefile</span>. For example,

    % cabal2pkg init foo-1.0 -f '+bar'

Running the command above in a pkgsrc directory
<span class="Pa">devel/hs-foo</span> downloads a Haskell package
`foo-1.0` from Hackage and initialises the current directory with pkgsrc
package files, whose <span class="Pa">Makefile</span> containing the
following line:

    CONFIGURE_ARGS+=    -f +bar

Applying package flags affects conditionals in
<span class="Pa">.cabal</span> files. Haskell packages can have optional
dependencies that can be toggled with package flags. Consider the
following snippet of a package description:

    flag lua
        description: Include support for scripting with Lua.
        manual: True
        default: False

    executable foo
        build-depends: aeson ^>= 2.2.1,
                       base   == 4.*
        if flag(foo)
            build-depends: lua ^>= 2.3.2

This package builds and installs an executable named `foo` which only
depends on packages `aeson` and `base`, but when a flag `lua` is enabled
it additionally depends on package `lua`. Without enabling this flag,
`cabal2pkg` would produce a <span class="Pa">Makefile</span> like this:

    # $NetBSD$

    DISTNAME=   foo-1.0
    CATEGORIES= devel

    # MAINTAINER, COMMENT, LICENSE, ...

    .include "../../converters/hs-aeson/buildlink3.mk"
    .include "../../mk/haskell.mk"
    .include "../../mk/bsd.pkg.mk"

However, when the flag is enabled it instead produces something like
this:

    # $NetBSD$

    DISTNAME=   foo-1.0
    CATEGORIES= devel

    # MAINTAINER, COMMENT, LICENSE, ...

    CONFIGURE_ARGS+=    -f +lua

    .include "../../converters/hs-aeson/buildlink3.mk"
    .include "../../lang/hs-lua/buildlink3.mk"
    .include "../../mk/haskell.mk"
    .include "../../mk/bsd.pkg.mk"

`--fill-column`=`COLUMN`  
Column beyond which line-wrapping should happen. Only used for
<span class="Pa">DESCR</span> files at the time of writing this manual
page.

`--ghc`=`FILE`  
The path to GHC executable. This is mostly used for locating the Haskell
package directory. Its default value is determined at the build time.

`--make`=`FILE`  
The path to BSD <span class="Xr">make(1)</span> command. Its default
value is determined at the build time, and the environment variable
`MAKE` affects it. See <span class="Sx">ENVIRONMENT</span> for details.

`-h`, `--help`  
Display a help message.

`--version`  
Display the version of `cabal2pkg`.

## Subcommands

`init`  
The `init` subcommand initialises the current directory (or whatever
directory specified with `-p`) with a newly created pkgsrc package. This
subcommand takes a single positional argument `PACKAGE-URI` specifying
which Haskell package to import. It can be one of the following:

- `http://` or `https://` URL of a package tarball. This is a preferred
  way of importing packages that are not registered to Hackage.

- `file://` URL of a package tarball on the local filesystem. This is
  mostly for debugging purpose and is generally unrecommended to use,
  because <span class="Pa">Makefile</span> generated from a local
  tarball will not have `MASTER_SITES`.

- The name of a package to retrieve from Hackage, in the form of `NAME`
  or `NAME`-`VERSION` (e.g. `foo-0.1.2`). If the version is omitted, the
  latest available (and non-deprecated) one will be chosen.

The `init` subcommand can additionally take the following options:

`-w`, `--overwrite`  
Allow the command to overwrite existing files. Without this option it
refuses to overwrite files, regardless of whether they have been
modified after being generated.

`update`  
The `update` subcommand updates an existing package by merging
differences between the current version and a newer one. Since this is a
3-way merge, changes may conflict. When that happens conflict markers
will be left on files and you will have to resolve them.

This subcommand optionally takes a single positional argument
`PACKAGE-URI` specifying which Haskell package to update to. Its syntax
is almost the same as that of the `init` subcommand, but there is a
single difference: a package to retrieve from Hackage needs to be
specified with only a `VERSION` but not with a `NAME`. When this
argument is omitted, the `update` subcommand attempts to retrieve the
latest available (and non-deprecated) version from Hackage.

The `update` subcommand can additionally take the following options:

`-f`, `--force`  
Perform the update forcefully. Without this option the `update`
subcommand refuses to update the package if any of the following
conditions are met, and this option overrides the refusal:

- The given new version is actually older than the current one.

- The given new version has been marked as deprecated on Hackage. This
  usually means that version has known defects and the upstream thinks
  it shouldn't be used.

- You are updating a package with a local tarball, which makes it lose
  its `MASTER_SITES`.

`-m` `STYLE`, `--merge`=`STYLE`  
Choose the style of conflict markers. `STYLE` can either be `rcs` (RCS
<span class="Xr">merge(1)</span>) or `diff3` (GNU
<span class="Xr">diff3(1)</span>) (default: `rcs`).

# ENVIRONMENT

The following environment variables affect the behaviour of `cabal2pkg`:

`MAKE`  
The name of, or the path to BSD <span class="Xr">make(1)</span> command
to use. If it's not defined `bmake` or `make` will be searched in the
environment variable `PATH`, with the former being preferred. This
variable only takes effect during the build time of `cabal2pkg`.

`NO_COLOR`  
`cabal2pkg` adopts the [NO_COLOR standard](https://no-color.org/). When
the variable is set to a non-empty string (regardless of the value),
coloured output gets disabled by default. The `--colour` option can
still override it.

`PKGMAINTAINER`, `REPLYTO`  
The default value of `MAINTAINER` in <span class="Pa">Makefile</span>,
with the former being preferred. Only used by the subcommand `init`.
`update` inherits whatever set in the current
<span class="Pa">Makefile</span>.

# FILES

`cabal2pkg` creates or updates the following files in a package
directory:

<span class="Pa">COMMIT_MSG</span>  
This file is created (or overwritten) when the option `-n` is *not*
given. On `init` it contains the generated contents of
<span class="Pa">DESCR</span>. On `update` it is generated by taking a
difference of old and new <span class="Pa">ChangeLog</span> of the
package in the hope of discovering updates. Usually this is a good guess
but it isn't guaranteed to be correct. ChangeLogs are typically marked
up with Markdown but no attempts are made to interpret it.

<span class="Pa">DESCR</span>  
This file is generated from the `description` field of a
<span class="Pa">.cabal</span> file. Haddock markup is interpreted and
rendered as a plain text.

<span class="Pa">Makefile</span>  
This file is generated mainly from a <span class="Pa">.cabal</span> file
but the pkgsrc tree and the Haskell package database are also consulted
during its generation.

`cabal2pkg` can handle conditional dependencies too; packages that
require different set of dependencies for each platform are represented
with <span class="Pa">Makefile</span> conditionals.

<span class="Pa">PLIST</span>  
On `init` this file is created merely as a stub because the only way to
generate it properly is to actually build the package, and `cabal2pkg`
does not do that. On `update` it is left unchanged.

<span class="Pa">buildlink3.mk</span>  
This file is generated from the same data source as that of
<span class="Pa">Makefile</span> but only when the package provides
libraries. In other words this file will not be generated if the package
only provides executables.

<span class="Pa">distinfo</span>  
This file is generated or updated by running `make distinfo` but on
`update` it will be left unchanged when the updated
<span class="Pa">Makefile</span> contains conflict markers, because
running `make` will certainly fail in that case.

# EXAMPLES

## Importing a package from Hackage

    % cd /usr/pkgsrc
    % mkdir devel/hs-foo
    % cd devel/hs-foo
    % cabal2pkg init foo-1.0

## Importing a package from a random site

    % cd /usr/pkgsrc
    % mkdir devel/hs-foo
    % cd devel/hs-foo
    % cabal2pkg init https://example.com/foo-1.0.tar.gz

## Updating a package from Hackage to the latest version

    % cd /usr/pkgsrc
    % cd devel/hs-foo
    % cabal2pkg update

## Updating a package from a random site

    % cd /usr/pkgsrc
    % cd devel/hs-foo
    % cabal2pkg update https://example.com/foo-2.0.tar.gz

# EXIT STATUS

`cabal2pkg` exits with 0 on success, and \>0 if an error occurs.

# SEE ALSO

<span class="Xr">pkgsrc(7)</span>, <span class="Xr">url2pkg(8)</span>

# AUTHORS

<span class="An">PHO</span> ⟨<pho@NetBSD.org>⟩ initially created the
tool and wrote this manual page.

# BUGS

Bugs and feature requests of `cabal2pkg` is tracked at
<https://github.com/depressed-pho/cabal2pkg/issues>
