#' Merge ORNL MODIS data with a PhenoCam time series
#' 
#' Combine PhenoCam time series with MODIS data for matching dates.
#' 
#' @param data a PhenoCam data file or data structure
#' @param trim logical, trim the MODIS data to the length of the
#' PhenoCam time series or include the whole Daymet time series (1980-current).
#' (default = \code{FALSE})
#' @param internal return a data structure if given a file on disk
#' (\code{TRUE} / \code{FALSE} = default)
#' @param out_dir output directory where to store data (default = tempdir())
#' @param product which MODIS product to query (character vector)
#' @param band which MODIS band(s) to include (character vector)
#' @return A PhenoCam data structure or file which combines PhenoCam time series
#' data with MODIS values (columns will be added). Data is queried from the
#' ORNL MODIS subsets service using the `MODISTools` package, please consult 
#' either sources on product and band names.
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
#' df <-  merge_modis(file.path(tempdir(),
#' "harvard_DB_1000_3day.csv"),
#' product = "MOD13Q1",
#' band = "250m_16_days_NDVI")
#' }

merge_modis = function(
  data,
  product,
  band,
  trim = FALSE,
  internal = TRUE,
  out_dir = tempdir()
  ){
  
  # if the data is not a data frame, load
  # the file (assuming it is a phenocam)
  # summary file, otherwise rename the
  # input data to df
  if(class(data) != "phenocamr"){
    if(file.exists(data)){
      data = read_phenocam(data)
    } else {
      stop("not a valid PhenoCam data frame or file")
    }
  }
  
  if(missing(product) || missing(band)){
    stop("No MODIS product or band specified!")
  }
  
  # start and end year of daymet downloads
  end_yr = format(Sys.time(), "%Y-%m-%d")
  
  # Download all available daymet data
  modis_data = try(
    MODISTools::mt_subset(
    product = product,
    band = band,
    end = end_yr,
    network = "PHENOCAM",
    site_id = data$site,
    internal = TRUE,
    progress = FALSE),
   silent = TRUE
  )
  
  if(inherits(modis_data,"try-error")){
    stop("MODIS data download failed!")
  }

  # selected centre pixel only
  px <- floor(max(modis_data$pixel)/2)
  modis_data <- modis_data[which(modis_data$pixel == px),]
  
  # apply multiplier
  if(modis_data$scale[1] != "Not Available"){
    modis_data$value <- modis_data$value * as.numeric(modis_data$scale)
  }
  
  # only retain date / band and value
  modis_data <- modis_data[,c("calendar_date","band","value")]
  
  # reshape the data if multiple bands were called,
  # rename columns
  modis_data <- as.data.frame(
    stats::reshape(modis_data,
          idvar = "calendar_date",
          timevar = "band",
          direction = "wide")
  )
  colnames(modis_data) <- gsub("value.","",colnames(modis_data))
  
  # convert to date object
  modis_data$date <- as.Date(modis_data$calendar_date)
  modis_data$calendar_date <- NULL
  
  # read phenocam data
  phenocam_data <- data$data
  phenocam_data$date <- as.Date(phenocam_data$date)
  
  # merge datasets
  phenocam_data = merge(phenocam_data,
                        modis_data,
                        by = "date",
                        all.x = TRUE)
  
  # put data back into the data structure
  data$data = phenocam_data
  
  # write the data to the original data frame or the
  # original file (overwrites the data!!!)
  if( !internal ){
    write_phenocam(data, out_dir = out_dir)
  } else {
    # if provided a data frame
    # return the original data frame, with flagged outliers
    class(data) = "phenocamr"
    return(data)
  }
}