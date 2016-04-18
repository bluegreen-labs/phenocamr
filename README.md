# PhenoCam R toolbox

The PhenoCam R toolbox is a collection of functions to facilitate the retrieval and processing of PhenoCam time series. If requested complementary Daymet or AmeriFlux (in development) data will be downloaded and merged with the PhenoCam data. The function will either return the original file as hosted on the PhenoCam server or the merged data file. The header of the original PhenoCam data file will be retained.

## Installation

clone the project to your home computer using the following command (with git installed)

	git clone https://khufkens@bitbucket.org/khufkens/phenocam-r-toolbox.git

alternatively, download the project using [this link](https://bitbucket.org/khufkens/phenocam-r-toolbox/get/master.zip).

Next, unzip the file (if necessary) and install the PhenoCam.tar.gz as a package in R.
	
## Use

To download data for a single site use the following syntax:

 	download.phenocam(site="harvard",vegetation="DB",frequency=3,daymet=TRUE)  

This will download all deciduous broadleaf (DB) PhenoCam time series for the "harvard" site. In addition corresponding data is downloaded from the Daymet server and merged with the PhenoCam data. 

To download data for multiple sites use the following syntax:

 	batch.download.phenocam(site="my_site_list.txt",vegetation="DB",frequency=3,daymet=TRUE)  

Here the site list is a list of different sites you want to query with a site name on each line (no header). This will download all deciduous broadleaf (DB) PhenoCam time series for all sites in your site list (if the vegetation type is available). In addition corresponding data is downloaded from the Daymet server and merged with the PhenoCam data. 


| Parameter | Description                                   |
|-----------|-----------------------------------------------|
| site      | sitename                                      |
| vegtation | abbreviation for the vegetation type (see below)|
| frequency | processing frequency (1 or 3 days)|
| daymet    | TRUE / FALSE (if true merge with Daymet data) |

The complete list of vegetation types is given below.



| Vegetation in ROI      | Abbreviation |
|------------------------|--------------|
| deciduous broadleaf    | DB           |
| evergreen needleleaf   | EN           |
| evergreen broadleaf    | EB           |
| grassland              | GR           |
| shrubland              | SH           |
| wetland                | WL           |
| agriculture / cropland | AG           |
| mixed canopy           | MX           |
| general canopy outline | XX           |
| no vegetation          | NV           |


## Notes

...

### Dependencies

The code depends on the following R packages: DaymetR ( [found here](https://bitbucket.org/khufkens/daymetr) )