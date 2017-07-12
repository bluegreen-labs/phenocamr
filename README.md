# phenocamr R toolbox

The phenocamr R toolbox is a collection of functions to facilitate the retrieval and processing of PhenoCam time series. If requested complementary [Daymet climate data](https://daymet.ornl.gov/) will be downloaded and merged with the PhenoCam data. The function will either return the original file as hosted on the PhenoCam server or the merged data file,using the same format, with the header of the original PhenoCam data file will be retained.

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

This will download all deciduous broadleaf (DB) PhenoCam time series for the "harvard" site at a 3-day time step. In addition, the data is processed to estimate phenological transition dates (phenophases) and written to file. For detailed overview of all use the R help browser.

## Citation and full description

An in depth description and worked example of the phenocamr R package can be found in

- Hufkens et al. (2017), An integrated phenology modelling framework in R. XYZ. xyz : xyz-xyz

Please cite the package using this reference.

## External Dependencies

The code depends on the following R package [DaymetR](https://khufkens.github.io/daymetr) which can be installed from the repository using:

```R
if(!require(devtools)){install.package(devtools)}
devtools::install_github("khufkens/daymetr")
```

# Citation

Hufkens K., Basler J. D., Milliman T. Melaas E., Richardson A.D. 2017 An integrated phenology modelling framework in R: Phenology modelling with phenor. in review

# Acknowledgements

This project was is supported by the National Science Foundation’s Macro-system Biology Program (award EF-1065029).
