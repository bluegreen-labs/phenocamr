# phenocamr

The phenocamr R package is a collection of functions to facilitate the retrieval and processing of PhenoCam time series. The package ensures proper post-processing of PhenoCam data in data products such as phenological transition dates. If requested complementary [Daymet climate data](https://daymet.ornl.gov/) will be downloaded and merged with the PhenoCam data for modelling purposes. For detailed overview of all functions and worked example we reference to the R help documentation and the manuscripts below (Hufkens et al. 2017, Richardson et al. 2017).

## Installation

Downloading the package from the github repository using:

```R
if(!require(devtools)){install.package(devtools)}
devtools::install_github("khufkens/phenocamr")
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

Hufkens K., Basler J. D., Milliman T. Melaas E., Richardson A.D. 2017 An integrated phenology modelling framework in R: Phenology modelling with phenor. In Review

Richardson, A.D., Hufkens, K., Milliman, T., Aubrecht, D.M., Chen, M., Gray, J.M., Johnston, M.R., Keenan, T.F., Klosterman, S.T., Kosmala, M., Melaas, E.K., Friedl, M.A., Frolking, S.  2017. Tracking vegetation phenology across diverse North American biomes using PhenoCam imagery. In Review.

## Acknowledgements

This project was is supported by the National Science Foundation’s Macro-system Biology Program (award EF-1065029).
