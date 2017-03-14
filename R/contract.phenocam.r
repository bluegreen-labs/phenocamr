#' Contracts the file from 1-day to a 3-day time step
#' reverting the expand.phenocam() routine in order
#' to save space and generate files as outlined in
#' the data paper. This routine is used as a post-production
#' measure, and not used in general manipulation
#' of the data as even 1-day files are sufficiently
#' small. Once expanded they are kept this way.
#'
#' @param file: a phenocam data file with a 3 day time step
#' @keywords time series, smoothing, phenocam
#' @export
#' @examples
#' # df = download.phenocam(site = "harvard",
#' #                        type = "DB",
#' #                        roi = "1",
#' #                        frequency = 3)
#'
#' # expand.phenocam("harvard_DB_0001_1day.csv")
#' # Overwrites the original file, increasing
#' # it's file size.
#'
#' # contract.phenocam("harvard_DB_0001_1day.csv")
#' # Contracts the file to it's original size, skipping
#' # two days.

contract.phenocam = function(filename) {
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