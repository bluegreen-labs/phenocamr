[![Build Status](https://travis-ci.org/khufkens/phenocamr.svg?branch=master)](https://travis-ci.org/khufkens/phenocamr) [![DOI](https://www.zenodo.org/badge/48943895.svg)](https://www.zenodo.org/badge/latestdoi/48943895)

# phenocamr

Phenocamr facilitates the retrieval and processing of PhenoCam time series. Post-processing of PhenoCam data includes outlier removal and the generation of data products such as phenological transition dates. If requested complementary [Daymet climate data](https://daymet.ornl.gov/) will be downloaded and merged with the PhenoCam data for modelling purposes. For a detailed overview of all functions and worked examples we refer to the R package documentation and publications by Hufkens et al. (2018) and Richardson et al. (2018).

## Installation

Downloading the package from the github repository using:

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
                  phenophases = TRUE)  
```

This will download all deciduous broadleaf (DB) PhenoCam time series for the "harvard" site at a 3-day time step. In addition, the data is processed to estimate phenological transition dates (phenophases) and written to file. For detailed overview of all functions and worked example we reference to the R help documentation and the manuscripts below.

## References

Hufkens K., Basler J. D., Milliman T. Melaas E., Richardson A.D. 2017. Methods in Ecology & Evolution. An integrated phenology modelling framework in R: Phenology modelling with phenor. (in press)

Richardson, A.D., Hufkens, K., Milliman, T., Aubrecht, D.M., Chen, M., Gray, J.M., Johnston, M.R., Keenan, T.F., Klosterman, S.T., Kosmala, M., Melaas, E.K., Friedl, M.A., Frolking, S.  2017. Scientific Data. Tracking vegetation phenology across diverse North American biomes using PhenoCam imagery. (in press)

## Acknowledgements

This project was supported by the National Science Foundation’s Macro-system Biology Program (award EF-1065029).
