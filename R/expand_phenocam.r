#' Expand a PhenoCam file from 3-day to a 1-day time step
#' All values will be filled with NA. This is usefull
#' to store subsequent interpolated (1-day data)
#' and increase consistency across processing.
#'
#' @param df a PhenoCam data frame
#' @param truncate year (numerical), limit the time series
#' to a particular year
#' @keywords time series, smoothing, phenocam
#' @export
#' @examples
#'
#' \donttest{
#' # download demo data
#' download_phenocam(site = "harvard$",
#'                   veg_type = "DB",
#'                   roi_id = "1",
#'                   frequency = "3")
#'
#' # Overwrites the original file, increasing
#' # it's file size.
#' expand_phenocam(paste0(tempdir(),"/harvard_DB_0001_3day.csv"))
#'
#' # Contracts the file to it's original size, skipping
#' # two days.
#' contract_phenocam(paste0(tempdir(),"/harvard_DB_0001_3day.csv"))
#' }

expand_phenocam = function(df, truncate = NULL) {
  
  # check validaty of the input
  if (is.data.frame(df)) {
    stop("not a PhenoCam data file")
  }
  
  # suppress warnings as it throws unnecessary warnings
  # messing up the feedback to the CLI
  header = try(readLines(df, n = 22), silent = TRUE)
  
  # directly read data from the server into data.table
  phenocam_data = utils::read.table(df, header = TRUE, sep = ",")
  phenocam_dates = as.Date(phenocam_data$date)
  
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
  all_years = format(all_dates, "%Y")
  all_doy = format(all_dates, "%j")
  
  # combine original data with missing dates filled in
  output = matrix(NA, length(all_dates), dim(phenocam_data)[2])
  output[which(all_dates %in% phenocam_dates), ] = as.matrix(phenocam_data)[phenocam_dates %in% all_dates,]
  output[, 1] = as.character(all_dates)
  output[, 2] = all_years
  output[, 3] = all_doy
  
  # writing the final data frame to file, retaining the original header
  utils::write.table(
    header,
    df,
    quote = FALSE,
    row.names = FALSE,
    col.names = FALSE,
    sep = ""
  )
  utils::write.table(
    t(matrix(colnames(phenocam_data))),
    df,
    quote = FALSE,
    row.names = FALSE,
    col.names = FALSE,
    append = TRUE,
    sep = ","
  )
  utils::write.table(
    output,
    df,
    quote = FALSE,
    row.names = FALSE,
    col.names = FALSE,
    sep = ",",
    append = TRUE
  )
}