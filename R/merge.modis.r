#' Merge MODIS data with a PhenoCam data file.
#' Merged data includes MODIS MCD43 BRDF data for
#' bands 1 - 3 and matching QA/QC data as well as
#' MODIS phenology (MCD12Q2) data
#' 
#' @param df: a PhenoCam data file (not a data frame!)
#' @keywords time series, PhenoCam, MODIS, data integration
#' @export
#' @examples
#' 
#' \dontrun{
#' # download demo data (do not smooth)
#' download.phenocam(site = "harvard",
#'                   vegetation = "DB",
#'                   roi_id = 1,
#'                   frequency = 3)
#' merge.modis("harvard_DB_0001_1day.csv")
#' }

merge.modis = function(df, size = c(0,0) ){
  
  # load data and check input parameters
    if (file.exists(df)) {
      
      # read header of the file
      header = readLines(df,n = 22)
      
      # unravel list
      header_data = strsplit(header,split=":")
      l = lapply(header_data,length)
      header_data = unlist(lapply(header_data[which(l>1)],"[[",2))
      
      # remove leading spaces
      header_data = gsub(" ","",header_data)
      
      # extract metadata from the header field
      sitename = header_data[1]
      veg_type = header_data[2]
      roi_id = as.numeric(header_data[3])
      frequency = header_data[9]
      lat = as.numeric(header_data[4])
      long = as.numeric(header_data[5])
      
      # read the original data
      df = utils::read.table(df, header = T, sep = ',')
      
    } else {
      stop("not a valid PhenoCam file")
    }
  
  # grab the current temporary directory of this session
  # this will be the location where the MODIS data is stored
  # and will be destroyed at the end of the session
  output_dir = tempdir()
  
  # create the location file used in downloading the MODIS data
  start.date = min(df$year)
  end.date = max(df$year)
  ID = sitename
  site = data.frame(lat, long, start.date, end.date, ID)
  
  # set phenology bands to download according to the latitude
  phenology_bands = c("Onset_Greenness_Minimum",
                      "Onset_Greenness_Decrease",
                      "Onset_Greenness_Maximum",
                      "Onset_Greenness_Increase")
  if (lat >= 0){
    phenology_bands = paste(phenology_bands,".Num_Modes_0",1,sep = "")
  } else {
    phenology_bands = paste(phenology_bands,".Num_Modes_0",2,sep = "")
  }
    
  # download the red, NIR and blue band
  MODISSubsets(
    LoadDat = site,
    Products = "MCD43A4",
    Bands = c(
      "Nadir_Reflectance_Band1",
      "Nadir_Reflectance_Band2",
      "Nadir_Reflectance_Band3"
    ),
    Size = size,
    StartDate = TRUE,
    SaveDir = output_dir
  )
  
  # download QA/QC data
  MODISSubsets(
    LoadDat = site,
    Products = "MCD43A2",
    Bands = "BRDF_Albedo_Quality",
    Size = size,
    StartDate = TRUE,
    SaveDir = output_dir
  )
  
  # downloading MODIS phenology
  MODISSubsets(
    LoadDat = site,
    StartDate = TRUE,
    Products = "MCD12Q2",
    Bands = phenology_bands,
    Size = size,
    StartDate = TRUE,
    SaveDir = output_dir
  )
  
  #files = list.files(output_dir,"*.asc")
  
  # Download all available daymet data
  #modis_status = try(, silent=TRUE)
  
  # # error trap the latency in the Daymet data releases
  # if(inherits(modis_status,"try-error")){
  #      stop(' MODIS data not available -- server issues / or location out of range') 
  # }
  # 
  # # write all data to file, first write the header then append the data
  # utils::write.table(
  #   header,
  #   filename,
  #   row.names = FALSE,
  #   col.names = FALSE,
  #   quote = FALSE
  # )
  # 
  # utils::write.table(
  #   df,
  #   filename,
  #   row.names = FALSE,
  #   col.names = TRUE,
  #   quote = FALSE,
  #   sep = ',',
  #   append = TRUE
  # )
  
}

#merge.modis("~/Downloads/harvard_DB_0001_3day.csv")