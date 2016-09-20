#' Expand a PhenoCam file from 3-day to a 1-day time step
#' All values will be filled with NA. This is usefull
#' to store subsequent interpolated (1-day data)
#' and increase consistency across processing.
#'
#' @param file: a PhenoCam data file with a 3 day time step
#' @keywords time series, smoothing, phenocam
#' @export
#' @examples
#'
#' df = download.phenocam(site="harvard",
#'                        type="DB",
#'                        roi="1",
#'                        frequency=3)
#'
#' expand.phenocam("harvard_DB_0001_1day_v4.csv")
#' # overwrites the original file, increasing
#' # it's file size. shrink the file using
#' # contract.phenocam()

expand.phenocam = function(df) {
  # check validaty of the input
  if (is.data.frame(df)) {
    stop("not a PhenoCam data file")
  }
  
  # suppress warnings as it throws unnecessary warnings
  # messing up the feedback to the CLI
  header = try(readLines(df, n = 22), silent = TRUE)
  
  # PUT ERROR TRAP IN HERE IF THE TRY STATEMENT FAILS
  
  # directly read data from the server into data.table
  phenocam_data = read.table(df, header = TRUE, sep = ",")
  
  # create phenocam dates string
  phenocam_dates = as.Date(phenocam_data$date)
  min_range = min(phenocam_dates)
  max_range = max(phenocam_dates)
  all_dates = seq(as.Date(min_range), as.Date(max_range), "days")
  all_years = format(all_dates, "%Y")
  all_doy = format(all_dates, "%j")
  
  # combine data with missing dates filled in
  output = matrix(NA, length(all_datephenocam_datas), dim(phenocam_data)[2])
  output[which(all_dates %in% phenocam_dates), ] = as.matrix(phenocam_data)
  output[, 1] = as.character(all_dates)
  output[, 2] = all_years
  output[, 3] = all_doy
  
  # writing the final data frame to file, retaining the original header
  write.table(
    header,
    df,
    quote = FALSE,
    row.names = FALSE,
    col.names = FALSE,
    sep = ""
  )
  write.table(
    t(matrix(colnames(phenocam_data))),
    df,
    quote = FALSE,
    row.names = FALSE,
    col.names = FALSE,
    append = TRUE,
    sep = ","
  )
  write.table(
    output,
    df,
    quote = FALSE,
    row.names = FALSE,
    col.names = FALSE,
    sep = ",",
    append = TRUE
  )
}