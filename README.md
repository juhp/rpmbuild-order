# rpmbuild-order

This package based on code from cabal-sort, sorts rpm package spec files
by build order.

    $ rpmbuild-order --help
    $ rpmbuild-order mycore.spec mylib.spec myapp.spec
    mylib
    mycore
    myapp