#' Expand a PhenoCam time series from 3-day to a 1-day time step
#' 
#' Necessary step to guarantee consistent data processing between 1 and 3-day
#' data products. Should rarely be used independent of `download_phenocam()`.
#'
#' @param data a PhenoCam file
#' @param truncate year (numerical), limit the time series
#' to a particular year (default = NULL)
#' @param internal return a data structure if given a file on disk
#' (\code{TRUE} / \code{FALSE} = default)
#' @param out_dir output directory where to store data (default = tempdir())
#' @return Expanded PhenoCam data structure or file, including 90 day padding
#' if requested.
#' @keywords time series, post-processing, phenocam
#' @export
#' @examples
#'
#' \donttest{
#' # download demo data
#' download_phenocam(site = "harvard$",
#'                   veg_type = "DB",
#'                   roi_id = "1000",
#'                   frequency = "3")
#'
#' # Overwrites the original file, increasing
#' # it's file size.
#' expand_phenocam(file.path(tempdir(),"harvard_DB_1000_3day.csv"))
#'
#' # Contracts the file to it's original size, skipping
#' # two days.
#' contract_phenocam(file.path(tempdir(),"harvard_DB_1000_3day.csv"))
#' }

expand_phenocam = function(data,
                           truncate = NULL,
                           internal = TRUE,
                           out_dir = tempdir()) {
  
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
  
  # split out data from read in or provided data
  # after contracting without any padding
  phenocam_data = contract_phenocam(data,
                                    internal = TRUE,
                                    no_padding = TRUE)$data
  
  # convert dates
  phenocam_dates = as.Date(phenocam_data$date)
  
  # remove dates that were filled before expanding again
  # this is similar to phenocam_contract() but includes
  # the removal of the padding
  
  # truncate the data if necessary
  max_date = max(phenocam_dates)
  
  # pad with 90 days (regardless)
  min_range = min(as.Date(phenocam_data$date)) - 90
  max_range = max(as.Date(phenocam_data$date)) + 90
  
  # set truncate date if value provided
  truncate_date = as.Date(ifelse(is.null(truncate),
                         max_date,
                         as.Date(sprintf("%s-12-31",truncate))),"1970-01-01")
  
  # if truncated, correct boundaries
  if ( max_date > truncate_date ) {
    phenocam_data = phenocam_data[which(as.Date(phenocam_data$date) <= truncate_date),]
    phenocam_dates = as.Date(phenocam_data$date)
    max_range = truncate_date + 90
  }
  
  # create vectors to populate final output with
  all_dates = seq(as.Date(min_range), as.Date(max_range), "days")
  all_years = as.integer(format(all_dates, "%Y"))
  all_doy = as.integer(format(all_dates, "%j"))
  
  # create data frame with dates to merge with original data
  all_dates = as.data.frame(as.character(all_dates))
  colnames(all_dates) = "date"
  
  output = merge(all_dates, phenocam_data, by = "date", all.x = TRUE)
  output$date = as.character(output$date)
  output$year = all_years
  output$doy = all_doy
  
  # stuff expanded data back into original data structure
  data$data = output
  
  # write the data to the original data frame or the
  # original file (overwrites the data!!!)
  if(on_disk | !internal ){
    write_phenocam(data, out_dir = out_dir)
  } else {
    # if provided a data frame
    # return the original data frame, with flagged outliers
    return(data)
  }
}