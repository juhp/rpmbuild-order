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
.
> rpmbuild-order C.spec A.spec
.
then the emitted order of packages may be wrong,
because rpmbuild-order does not get to know the dependency of C on B.
Even if the order is correct, B.spec is missing in the output
and thus the list of spec files cannot immediately be used
for a sequence of builds.

