!!! This code is in active development and should currently not be used !!!

# PhenoCam R toolbox

The PhenoCam R toolbox is a collection of functions to facilitate the retrieval and processing of PhenoCam time series. If requested complementary Daymet or AmeriFlux (in development) data which will be downloaded and merged with the PhenoCam data. The function will either return the original file as hosted on the PhenoCam server or the merged data file,using the same format, with the header of the original PhenoCam data file will be retained.

## Installation

You can quick install the package by installing the following dependencies

```R
install.packages(c("zoo","changepoint","data.table","curl","RCurl","DT","shiny","shinydashboard","leaflet","plotly","devtools"))
```

and downloading the package from the github repository

```R
require(devtools)
install_github("khufkens/phenocamr")
```

## Use

To download data for a single site use the following syntax:

```R
download.phenocam(site="harvard",vegetation="DB",frequency=3,daymet=TRUE)  
```

This will download all deciduous broadleaf (DB) PhenoCam time series for the "harvard" site. In addition corresponding data is downloaded from the Daymet server and merged with the PhenoCam data. For detailed function overview use the R help browser

### Notes / Dependencies

The code depends on the following R packages: ( [DaymetR](https://khufkens.github.io/daymetr) ) and ( [AmerifluxR](https://khufkens.github.io/amerifluxr) )

# Acknowledgements

This project was is supported by the National Science Foundation’s Macro-system Biology Program (award EF-1065029).
