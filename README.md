[![Build Status](https://travis-ci.org/juhp/rpmbuild-order.png)](https://travis-ci.org/juhp/rpmbuild-order)

# rpmbuild-order

This package based on code from [cabal-sort](http://hackage.haskell.org/package/cabal-sort), sorts rpm package spec files by build order.

    $ rpmbuild-order --help
    $ rpmbuild-order mycore mylib myapp
    mylib
    mycore
    myapp

## Known problems
Given packages A, B, C, where C depends on B, and B depends on A,
and you call

    rpmbuild-order C.spec A.spec

then the output may be wrong if C does not have a direct dependency on A.
Even if the order is correct, B.spec is missing in the output
and thus in this case the list of packages cannot be reliably used
for a sequence of builds.

