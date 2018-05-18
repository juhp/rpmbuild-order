[![Build Status](https://travis-ci.org/juhp/rpmbuild-order.png)](https://travis-ci.org/juhp/rpmbuild-order)

# rpmbuild-order

This package based on code from [cabal-sort](http://hackage.haskell.org/package/cabal-sort), sorts rpm package spec files by build order.

    $ rpmbuild-order --help
    $ rpmbuild-order mycore.spec mylib.spec myapp.spec
    mylib
    mycore
    myapp