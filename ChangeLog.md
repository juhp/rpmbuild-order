# 0.4.2.1 (2020-09-10)
- 'sort': fix arg handled which was causing reverse ordering!
- add a sort test for the installed rpmbuild-order
- drop the erroneous dependency on Cabal

# 0.4.2 (2020-08-04)
- support options for rpmspec (suggested by QuLogic)
  eg `rpmbuild-order sort --rpmopt --with=bootstrap ...`
- Graph: add createGraphRpmOpts and createGraph''
- Order: add dependencySortRpmOpts
- add a newline between cycles

# 0.4.1 (2020-08-03)
- no longer assume pkg dir = pkg name (though still optimize for it)
- output any subcycles for cyclic graph (suggested by QuLogic)

# 0.4.0 (2020-07-29)
- performance: rework just to use String and only parse spec files once
  and also use faster PatriciaTree.Gr
  On about 500 packages roughly twice as fast as 0.3.1
- sort now defaults to outputting separate dependency stacks, with options for combined, connected, and independent packages only
- new 'layers' command outputs packages in ordered dependency independent layers
- new 'chain' command outputs Fedora chain-build format
- new 'leaves' commands to list outer leaf packages
- new 'roots' commands lists lowest dependencies
- new library exposed with 2 modules: low-level Graph and high-level Order:
  - Distribution.RPM.Build.Order provides: dependencySort, dependencySortParallel,
    dependencyLayers, sortGraph output
  - Distribution.RPM.Build.Graph provides: createGraph, dependencyNodes,
    subgraph', packageLayers, etc
- graph Nodes are now only labelled by package/spec filepath
  and no longer carry redundant dependency lists
- add a basic testsuite for the library

# 0.3.1 (2020-07-04)
- fix detection of circular dependencies (bug introduced in 0.3)

# 0.3 (2019-10-10)
- check package provides instead of rpms
- use ByteString for packages (provides)
- move to simple-cmd-args (optparse-applicative)
- add --lenient for rpmspec failures

# 0.2 (2018-08-17)
- error if no spec file
- filter out hidden files from deps search
- add --version
- generate manpage with help2man

# 0.1 (2018-07-18)
- initial release with sort, deps and rdeps (reverse dependencies) commands
