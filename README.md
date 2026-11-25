# The `libltdl` Package  [![Hackage](https://img.shields.io/hackage/v/libltdl.svg)](https://hackage.haskell.org/package/libltdl) [![Actions Status: haskell-ci](https://github.com/mainland/libltdl/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/mainland/libltdl/actions?query=workflow%3Ahaskell-ci)

The package provides a Haskell interface to the `libltdl` library that is part
of [GNU Libtool](https://www.gnu.org/software/libtool/). It supports libtool
modules, extension searches, configurable search paths, and resident modules.
For ordinary shared-library loading on POSIX systems,
[`System.Posix.DynamicLinker`](https://hackage.haskell.org/package/unix/docs/System-Posix-DynamicLinker.html)
is another option.

Install the libltdl library and development headers before building. On Debian
or Ubuntu, install `libltdl-dev`. With Homebrew, install `libtool`. When the
library is installed under a prefix that the C compiler does not search, pass
its directories to Cabal. For Homebrew:

```sh
cabal build --extra-include-dirs=$(brew --prefix)/include --extra-lib-dirs=$(brew --prefix)/lib
```

Build, test, and document the package with:

```sh
cabal build all
cabal test all
cabal haddock all
```

On Linux, the test suite opens `libltdl.so` by name. The dynamic loader must be
able to find it, either in a default library directory, where the `libltdl-dev`
package installs it, or through `LD_LIBRARY_PATH`.

The package supports GHC 8.0 and later. CI tests the versions listed in
`libltdl.cabal`.

See the [API documentation](https://hackage.haskell.org/package/libltdl/docs/Foreign-LibLTDL.html)
for an example of loading with a shared lock, the synchronization requirements,
and the lifetime of module handles and symbols.

## AI assistance

All contributions included in releases through version `0.1.1.2` were made
without AI assistance. AI tools were used in development, review, and release
preparation after that version, through version `0.2.0`. The maintainer remains
responsible for this release.
