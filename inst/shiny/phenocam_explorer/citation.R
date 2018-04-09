tags$html(
  tags$head(
    tags$title('Citation page')
  ),
  tags$body(
    h3("About the data"),
    p("Vegetation phenology controls the seasonality of many ecosystem processes, as well as numerous biosphere-atmosphere feedbacks. Phenology is also highly sensitive to climate change and variability. Here we present a series of datasets, together consisting of almost 750 years of observations, characterizing vegetation phenology in diverse ecosystems across North America. Our data are derived from conventional, visible-wavelength, automated digital camera imagery collected through the PhenoCam network. For each archived image, we extracted RGB (red, green, blue) colour channel information, with means and other statistics calculated across a region-of-interest (ROI) delineating a specific vegetation type. From the high-frequency (typically, 30 minute) imagery, we derived time series characterizing vegetation colour, including “canopy greenness”, processed to 1- and 3-day intervals. For ecosystems with one or more annual cycles of vegetation activity, we provide estimates, with uncertainties, for the start of the “greenness rising” and end of the “greenness falling” stages. The database can be used for phenological model validation and development, evaluation of satellite remote sensing data products, benchmarking earth system models, and studies of climate change impacts on terrestrial ecosystems."),
    
    h3("PhenoCam Fair Use Data Policy"),
    p("Since its inception, the objective of the PhenoCam network has been to serve as a repository for phenologically-relevant, digital, repeat (time-lapse) imagery, and to make that imagery, and derived data products, freely available to a wide array of third-party data end-users, including researchers, educators, and the general public. Thus, imagery from the PhenoCam archive is made publicly available, without restriction, and we encourage you to download imagery and datasets for use in your own research and teaching."),
    p("We request that all publications using the dataset (1) properly cite the processed PhenoCam dataset (and underlying image dataset, if applicable), the associated data descriptor, and other relevant publications; and (2) include both a general acknowledgement for the PhenoCam project and the appropriate site-specific acknowledgements for every site used in your analysis. The text for these acknowledgments is included in the metadata file associated with each PhenoCam site."),
    
    h3("Citing the package"),
    p("Data generated using the package should be acknowledged using the following citation and those specified under the fair use policy (https://phenocam.sr.unh.edu/webcam/fairuse_statement/):"),
    p(tags$li("Hufkens et al. (2018). Combining curated PhenoCam data and three R packages into an integrated phenology modelling framework. Methods in Ecology & Evolution. 9: 1-10."))
    
)
)
