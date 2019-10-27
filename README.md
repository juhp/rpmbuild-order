[![Build Status](https://travis-ci.org/juhp/rpmbuild-order.png)](https://travis-ci.org/juhp/rpmbuild-order)
[![Hackage](http://img.shields.io/hackage/v/rpmbuild-order.png)](http://hackage.haskell.org/package/rpmbuild-order)
[![license](https://img.shields.io/badge/license-BSD-brightgreen.svg)](https://opensource.org/licenses/BSD-3-Clause)

# rpmbuild-order

This package based on code from [cabal-sort](http://hackage.haskell.org/package/cabal-sort), sorts rpm package spec files by build order.

    $ rpmbuild-order --help
    $ rpmbuild-order sort mycore mylib myapp
    mylib
    mycore
    myapp

The arguments passed can either be directories named after the package, or spec files.

By default it outputs the package names, but it can also output
the spec filenames or directory paths for easier scripting.

Using the rpmbuild-order `deps` and `rdeps` commands the ordered
dependencies and reverse dependencies of a package can be obtained
from the current set of checked out package sources.

## Known problems
1. Given packages A, B, C, where C depends on B, and B depends on A,
and you call

    rpmbuild-order sort C.spec A.spec

then the output may be wrong if C does not have a direct dependency on A.
Even if the order is correct, B is missing in the output
and thus in this case the list of packages cannot be reliably used
for a sequence of builds.

However the `deps` and `rdeps` commands take
other neighbouring package directories into account.

2. repoquery is not used to resolve Requires or filelists for Provides.
So if a package BuildRequires a file, it will not be resolved to a package.
This may get addressed in a future version.
