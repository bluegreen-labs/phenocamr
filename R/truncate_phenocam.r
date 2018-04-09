#' Truncate a PhenoCam file using a given year as last valid year.
#' expand.phenocam() provides a similar functionality and is to be
#' prefered. This function remains as it might serve a purpose to
#' some.
#'
#' @param df a PhenoCam data frame
#' @param year the last valid year, discard the rest
#' @keywords time series, smoothing, phenocam
#' @export
#' @examples
#'
#' \donttest{
#' download_phenocam(site = "harvard$",
#'                   vegetation = "DB",
#'                   roi_id = "1",
#'                   frequency = "3")
#' 
#' # read data as data frame
#' df = utils::read.csv(paste0(tempdir(),"/harvard_DB_0001_3day.csv"))
#' 
#' # overwrites the original file, increasing
#' # decreasing the file size a given year as maximum.
#' truncate_phenocam(df, year = 2015)
#' }

truncate_phenocam = function(df,
                             year = 2015) {
  
  # check validaty of the input
  if (is.data.frame(df)) {
    stop("not a PhenoCam data file")
  }
  
  # suppress warnings as it throws unnecessary warnings
  # messing up the feedback to the CLI
  header = try(readLines(df, n = 22), silent = TRUE)
  
  # directly read data from the server into data.table
  phenocam_data = utils::read.table(df, header = TRUE, sep = ",")
  
  # truncate the data using a given year as last viable year
  # of data points
  phenocam_data = phenocam_data[phenocam_data$year <= year,]
  
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
    phenocam_data,
    df,
    quote = FALSE,
    row.names = FALSE,
    col.names = FALSE,
    sep = ",",
    append = TRUE
  )
}