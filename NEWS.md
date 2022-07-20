# phenocamr 1.1.5

* Fix shiny + plotly colour bug
* github actions CI migration
* migration to new server URLs

# phenocamr 1.1.4

* adding local processing support with `process_phenocam()`
* ensuring phenocamr classes are maintained between processing steps
* added `merge_modis()` and `merge_daymet()` to provide additional data streams
* additional bug fixes and error traps
* R >= 3.6
* bluegreen-labs migration

# phenocamr 1.1.3

* use of bartlett site in unit checks
  * smaller files do not time out on CRAN
* check server online status in unit checks

# phenocamr 1.1.2

* critical bug fix in contract_phenocam()
* read_phenocam() allows "3" as a sitename component

# phenocamr 1.1.1

* package version requirement for daymetr (>= 1.3.2) due to Daymet server changes
* adding in r-hub checks for common platforms (osx, windows)
* move from curl to httr for stable Windows support
* fixed AICc1 formula error
* syntax, documentation fixes

# phenocamr 1.1.0

* New meta-data functions list_rois() and list_sites()
* Added a `NEWS.md` file to track changes to the package
* Included code coverage checks and status badge [![codecov](https://codecov.io/gh/khufkens/phenocamr/branch/master/graph/badge.svg)](https://codecov.io/gh/khufkens/phenocamr)
* Travis CI warnings == errors
* Added vignettes as additional documentation
* CRAN release

# phenocamr 1.0

* First github release, and version used in the PhenoCam data paper (Richardson et al. 2018)
