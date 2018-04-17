#' Truncate a PhenoCam file using a given year as last valid year.
#' expand_phenocam function provides a similar functionality and is to be
#' prefered. This function remains as it might serve a purpose to
#' some.
#'
#' @param data a PhenoCam file or data frame
#' @param year the last valid year, discard the rest
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
#' # overwrites the original file, increasing
#' # decreasing the file size a given year as maximum.
#' truncate_phenocam(paste0(tempdir(),"/harvard_DB_0001_3day.csv"),
#'                   year = 2015)
#' }

truncate_phenocam = function(filename,
                             year = 2015) {
  
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
  
  # split out data
  df = data$data
  
  # truncate the data using a given year as last viable year
  # of data points
  df = df[df$year <= year,]
  
  # put data back into original data structure
  data$data = df
  
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