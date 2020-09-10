[![Build Status](https://travis-ci.org/juhp/rpmbuild-order.png)](https://travis-ci.org/juhp/rpmbuild-order)
[![Hackage](http://img.shields.io/hackage/v/rpmbuild-order.png)](http://hackage.haskell.org/package/rpmbuild-order)
[![license](https://img.shields.io/badge/license-BSD-brightgreen.svg)](https://opensource.org/licenses/BSD-3-Clause)

# rpmbuild-order

This package originally based on code from [cabal-sort](http://hackage.haskell.org/package/cabal-sort), sorts rpm package spec files by build order.

    $ rpmbuild-order --help
    $ rpmbuild-order sort mycore mylib myapp

    mylib mycore myapp
    $

The arguments passed can either be directories containing the package, or spec files.

If the dependency graph has cycles then an error will be output with
a list of cycles and any subcycles.

Using the rpmbuild-order `deps` and `rdeps` commands the ordered
dependencies and reverse dependencies of a package can be obtained
from the current set of checked out package sources.

## Library
As of version 0.4, a library is also provided.

There are two modules:

- `Distribution.RPM.Build.Graph` provides lower level functions for generating
  RPM dependency graphs
- `Distribution.RPM.Build.Order` provides higher level functions for
  sorting packages in build dependency orders and output.

Please see the documentation for more details.

## Notes and known problems
1. Given packages A, B, C, where C depends on B, and B depends on A,
and you call

    rpmbuild-order sort C.spec A.spec

then the output may be wrong if C does not have a direct dependency on A.
Even if the order is correct, B is missing in the output
and thus in this case the list of packages cannot be reliably used
for a sequence of builds.

However the `deps` and `rdeps` commands take
other neighbouring package directories into account.

2. repoquery is not used to resolve meta-dependencies or files to packages.
So if a package BuildRequires a file, it will not be resolved to a package.
This may get addressed some day, but file dependencies seem less common for
BuildRequires than Requires.

3. rpmspec is used to parse spec files (for macro expansion etc):
so missing macros packages can lead to erroneous results in some cases.
