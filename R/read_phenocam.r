#' Read PhenoCam time series data
#' 
#' Reads PhenoCam data into a nested list, preserving
#' header data and critical file name information.
#' 
#' @param filename a PhenoCam data file
#' @return A nested data structure including site meta-data, the full
#' header and the data as a `data.frame()`.
#' @export
#' @examples
#'
#' \dontrun{
#' # download demo data (do not smooth)
#' download_phenocam(site = "harvard$",
#'                   veg_type = "DB",
#'                   roi_id = "1000",
#'                   frequency = "3",
#'                   smooth = FALSE)
#'
#' # read the phenocamo data file
#' df = read_phenocam(file.path(tempdir(),"harvard_DB_1000_3day.csv"))
#' 
#' # print data structure
#' print(summary(df))
#' 
#' # write the phenocamo data file
#' write_phenocam(df, out_dir = tempdir())
#' }

read_phenocam <- function(filename){
  
  # read and format header, read past the header (should the length)
  # change in the future with a few lines this then does not break
  # the script
  header = try(readLines(filename, n = 30), silent = TRUE)
  header = header[grepl("#", header)]
  header = strsplit(header, ":")
  
  # read in descriptor fields
  descriptor = unlist(lapply(header, function(x){
    x[1]
  }))
  
  # read in data values
  values = unlist(lapply(header, function(x){
    x <- unlist(x)
    if(length(x)<=1){
      return(NA)
    } 
    if(length(x) == 2){
      x[2]
    } else {
      paste(x[2:length(x)], collapse = ":")
    }
  }))
  
  # remove leading space
  values = gsub(" ","",values)
  
  # assign names to values
  names(values) = descriptor
  
  # grab filename elements
  veg_type = as.character(values["# Veg Type"])
  roi_id = sprintf("%04d", as.numeric(values["# ROI ID Number"]))
  site = as.character(values["# Site"])
  lat = as.numeric(values['# Lat'])
  lon = as.numeric(values['# Lon'])
  elev = as.numeric(values['# Elev'])
  solar_elev_min = as.numeric(values['# Solar Elevation Min'])
  
  # set frequency
  if(!grepl("ROI color", names(values[2]))){
    frequency = ifelse(grepl("3-day", names(values[2])), "3day", "1day")
  } else {
    frequency = "roistats"
  }
  
  # read the time series data
  data = utils::read.table(filename,
                           header = TRUE,
                           sep = ",",
                           stringsAsFactors = FALSE)
  
  # format final output as a nested list of class phenocamr
  output = list(
    "site" = site,
    "veg_type" = veg_type,
    "roi_id" = roi_id,
    "frequency" = frequency,
    "lat" = lat,
    "lon" = lon,
    "elev" = elev,
    "solar_elev_min" = solar_elev_min,
    "header" = values,
                "data" = data)
 
  # set proper phenocamr class
  class(output) = "phenocamr"
  return(output)
}
