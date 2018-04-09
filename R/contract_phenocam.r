#' Contracts the file from 1-day to a 3-day time step
#' reverting the expand.phenocam() routine in order
#' to save space and generate files as outlined in
#' the data paper. This routine is used as a post-production
#' measure.
#'
#' @param filename a phenocam data file with a 3 day time step
#' @keywords time series, smoothing, phenocam
#' @export
#' @examples
#' 
#' \donttest{
#' # download demo data
#' download_phenocam(site = "harvard$",
#'                   vegetation = "DB",
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

contract_phenocam = function(filename) {
  
  # check validaty of the input
  if (is.data.frame(filename)) {
    stop("not a PhenoCam data file")
  }
  
  # suppress warnings as it throws unnecessary warnings
  # messing up the feedback to the CLI
  header = try(readLines(filename, n = 22), silent = TRUE)
  
  # directly read data from the server into data.table
  data = utils::read.table(filename, header = TRUE, sep = ",")
  
  # drop the lines which should be empty
  loc = seq(2, 366, 3)
  data = data[which(data$doy %in% loc), ]
  
  # replace all -9999 values with NA
  # for consistency
  data[data == -9999] = NA
  
  # writing the final data frame to file, retaining the original header
  utils::write.table(
    header,
    filename,
    quote = FALSE,
    row.names = FALSE,
    col.names = FALSE,
    sep = ""
  )
  utils::write.table(
    data,
    filename,
    quote = FALSE,
    row.names = FALSE,
    col.names = TRUE,
    sep = ",",
    append = TRUE
  )
}