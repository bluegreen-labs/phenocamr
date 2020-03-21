#' Merge Daymet data with a PhenoCam time series
#' 
#' Combine PhenoCam time series with matching climatological variables from
#' Daymet.
#' 
#' @param data a PhenoCam data file or data structure
#' @param trim logical, trim the daymet data to the length of the
#' PhenoCam time series or include the whole Daymet time series (1980-current).
#' (default = \code{FALSE})
#' @param internal return a data structure if given a file on disk
#' (\code{TRUE} / \code{FALSE} = default)
#' @param out_dir output directory where to store data (default = tempdir())
#' @return A PhenoCam data structure or file which combines PhenoCam time series
#' data with Daymet based climate values (columns will be added).
#' @keywords time-series data-fusion
#' @export
#' @examples
#' 
#' \dontrun{
#' # download demo data
#' download_phenocam(site = "harvard$",
#'                   veg_type = "DB",
#'                   roi_id = "1000",
#'                   frequency = "3")
#'
#' # merge data with daymet data
#' merge_daymet(file.path(tempdir(),"harvard_DB_1000_3day.csv"))
#' }

merge_daymet  = function(data,
                         trim = FALSE,
                         internal = TRUE,
                         out_dir = tempdir()){
  
  # if the data is not a data frame, load
  # the file (assuming it is a phenocam)
  # summary file, otherwise rename the
  # input data to df
  if(class(data) != "phenocamr"){
    if(file.exists(data)){
      data = read_phenocam(data)
      on_disk = TRUE
    } else {
      stop("not a valid PhenoCam data frame or file")
    }
  } else {
    on_disk = FALSE
  }
  
  # start and end year of daymet downloads
  start_yr = 1980
  end_yr = as.numeric(format(Sys.time(), "%Y")) - 1
  
  # Download all available daymet data
  daymet_status = try(daymetr::download_daymet(
    site = data$site,
    lat = data$lat,
    lon = data$lon,
    end = end_yr,
    internal = TRUE,
    silent = TRUE
  ),
  silent = TRUE
  )
   
  # error trap the latency in the Daymet data releases
  if(inherits(daymet_status,"try-error")){
    if (grepl("check coordinates", daymet_status)){
      
      # reset end year
      end_yr = end_yr - 1
      
      # download daymet data
      daymet_status = try(daymetr::download_daymet(
        site = data$site,
        lat = data$lat,
        lon = data$lon,
        end = end_yr,
        internal = TRUE,
        silent = TRUE
      ),
      silent = TRUE)
      
      if (inherits(daymet_status, "try-error")){
        stop(' Daymet data not available -- server issues / or location out of range') 
      }
    }
  }

  # read in daymet data
  daymet_data = daymet_status$data

  # create date strings
  daymet_data$date = as.Date(sprintf("%s-%s",
                                 daymet_data$year,
                                 daymet_data$yday),
                         "%Y-%j")
  
  # subset only valid columns
  daymet_data$year <- NULL
  daymet_data$yday <- NULL
  
  # read phenocam data
  phenocam_data <- data$data
  phenocam_data$date <- as.Date(phenocam_data$date)
  
  # remove old data
  l <- grepl("\\.\\.",colnames(phenocam_data))
  phenocam_data[,l] <- NULL
  
  # merge datasets
  phenocam_data = merge(phenocam_data,
                        daymet_data,
                        by = "date")

  # put data back into the data structure
  data$data = phenocam_data
  
  # write the data to the original data frame or the
  # original file (overwrites the data!!!)
  if(on_disk | !internal ){
    write_phenocam(data, out_dir = out_dir)
  } else {
    # if provided a data frame
    # return the original data frame, with flagged outliers
    class(data) = "phenocamr"
    return(data)
  }
}