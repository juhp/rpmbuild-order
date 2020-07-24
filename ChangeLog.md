# 0.4.0 (2020-07-24)
- performance: rework just to use String and only parse spec files once
- sort now defaults to parallel output, with options for combined, connected, and separated output
- new 'layers' command outputs packages in ordered independent layers
- new 'chain' command output in fedora chain-build format
- new 'leaves' commands to list outer leaf packages
- new 'roots' commands lists lowest dependencies
- expose a library with 2 modules: low-level Graph and high-level Order:
  - Distribution.RPM.Build.Order provides: dependencySort, dependencySortParallel,
    dependencyLayers
  - Distribution.RPM.Build.Graph provides: createGraph, createGraphNodes,
    subgraph, packageLayers
- graph Nodes now only labelled by spec filepath
  and no longer carry redundant dependency list

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
