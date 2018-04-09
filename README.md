[![Build Status](https://travis-ci.org/khufkens/phenocamr.svg?branch=master)](https://travis-ci.org/khufkens/phenocamr)
[![codecov](https://codecov.io/gh/khufkens/phenocamr/branch/master/graph/badge.svg)](https://codecov.io/gh/khufkens/phenocamr)

# phenocamr

Facilitates the retrieval and post-processing of PhenoCam time series. The post-processing of PhenoCam data includes outlier removal and the generation of data products such as phenological transition dates. If requested complementary [Daymet climate data](https://daymet.ornl.gov/) will be downloaded and merged with the PhenoCam data for modelling purposes. For a detailed overview of the assumptions made during post-processing I refer publications by Hufkens et al. (2018) and Richardson et al. (2018). Please cite the Hufkens et al. (2018) reference when using the package. A worked example is included below and in the package vignette.

## Installation

### stable release

To install the current stable release use a CRAN repository:

```R
install.packages("phenocamr")
library(phenocamr)
```

### development release

To install the development releases of the package run the following commands:

```R
if(!require(devtools)){install.package(devtools)}
devtools::install_github("khufkens/phenocamr")
library(phenocamr)
```

## Use

To download data for a single deciduous broadleaf forest site (harvard) use the following syntax:

```R
download_phenocam(site = "harvard",
                  vegetation = "DB",
                  frequency = 3,
                  phenophases = TRUE,
                  out_dir = "~")
```

This will download all deciduous broadleaf (DB) PhenoCam time series for the "harvard" site at a 3-day time step into your home directory. In addition, the data is processed to estimate phenological transition dates (phenophases) and written to file. For detailed overview of all functions and worked example we reference to the R help documentation and the manuscripts below.

## References

Hufkens K., Basler J. D., Milliman T. Melaas E., Richardson A.D. 2018 [An integrated phenology modelling framework in R: Phenology modelling with phenor. Methods in Ecology & Evolution](http://onlinelibrary.wiley.com/doi/10.1111/2041-210X.12970/full), 9: 1-10.

Richardson, A.D., Hufkens, K., Milliman, T., Aubrecht, D.M., Chen, M., Gray, J.M., Johnston, M.R., Keenan, T.F., Klosterman, S.T., Kosmala, M., Melaas, E.K., Friedl, M.A., Frolking, S. 2017. [Tracking vegetation phenology across diverse North American biomes using PhenoCam imagery](https://www.nature.com/articles/sdata201828). Scientific Data, 5, 180028.

## Acknowledgements

This project was is supported by the National Science Foundation’s Macro-system Biology Program (awards EF-1065029 and EF-1702697).
