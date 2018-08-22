I have read and agree to the the CRAN policies at
http://cran.r-project.org/web/packages/policies.html

## test environments, local, CI and r-hub

- local OSX / Ubuntu 16.04 install on R 3.4.4
- Ubuntu 14.04 on Travis-CI (devel / release)
- codecove.io code coverage at ~89%
- r-hub additional checks (windows, with transitive package warning, macos clean)

## local / Travis CI R CMD check results

0 errors | 0 warnings | 0 notes

## r-hub R CMD check results for:

- CentOS 6, stock R from EPEL
- Ubuntu Linux 16.04 LTS, R-release, GCC
- Debian Linux, R-release, GCC

0 errors | 0 warnings | 1 note

Namespaces in Imports field not imported from:
‘DT’ ‘leaflet’ ‘plotly’ ‘shinydashboard’
All declared Imports should be used.

Shiny application is not tested so these packages are not used. Hence the note.

- Fedora Linux, R-devel, GCC

0 errors | 1 warning | 1 note

Not same as above, the warning pertains to the lack of an X11 device.
This bug is documented here: https://github.com/r-hub/rhub/issues/92

- Windows Server 2008 R2 SP1, R-release, 32/64 bit

Does not build cleanly, as the downloads in unit checks time out. Again,
this is a server issue. I tried to patch by moving from curl to httr, while
specifiying a long timeout (30s). This seems not to work.

