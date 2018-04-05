#' Calculate GRVI for a PhenoCam file or data frame
#' 
#' @param df a PhenoCam data file or data frame (when using a file provide a
#' full path if not in the current working directory)
#' @param par grvi parameters (weights)
#' @keywords time series, smoothing, phenocam
#' @export
#' @examples
#' 
#' \dontrun{
#' # with defaults, outputting a data frame
#' # with smoothed values, overwriting the original
#' 
#' # download demo data (do not smooth)
#' download_phenocam(site = "harvard",
#'                   vegetation = "DB",
#'                   roi_id = 1,
#'                   frequency = 3,
#'                   smooth = FALSE)
#' 
#' # calculate and append the GRVI for a file (and overwrite the original)
#' grvi("harvard_DB_0001_1day.csv")
#' 
#' # the function also works on a PhenoCam data frame
#' # but you will lose the extensive header in the process
#' df = read.csv("harvard_DB_0001_1day.csv")
#' df = grvi(df)
#' }

grvi = function(df,
                par=c(1,1,1)) {
  
  # read parameters into readable formats
  a = par[1]
  b = par[2]
  c = par[3]
  
  # check if it's a filename or data frame
  df_check = is.data.frame(df)

  # if the data is not a data frame, load
  # the file (assuming it is a phenocam)
  # summary file, otherwise rename the
  # input data to df
  if (!df_check) {
    if (file.exists(df)) {
      # create filename
      filename = df
      
      # read data file
      header = try(readLines(df, n = 22), silent = TRUE)
      df = utils::read.table(df, header = TRUE, sep = ",")
      
      if (grepl("timeseries", filename)){
        # select for solar elevation > 10
        df = df[df$solar_elev > 10,] 
      }
      
      # strip out necessary data into readable variables
      green = df$g_mean
      red = df$r_mean
      blue = df$b_mean
      
      # calculate the GRVI
      df$grvi = (green * a - red * b  - blue * c) /
        (green * a + red * b + blue * c)
      
    } else{
      stop("not a valid PhenoCam data frame or file")
    }
    
  } else {
    # strip out necessary data into readable variables
    green = df$g_mean
    red = df$r_mean
    blue = df$b_mean
    
    # calculate the GRVI
    df$grvi = (green * a - red * b  - blue * c) /
      (green * a + red * b + blue * c)
  }
  
  # if the data is not a data frame, write
  # to the same file else return df
  if (!df_check) {
    # writing the final data frame to file, 
    # retaining the original header
    utils::write.table(
      header,
      filename,
      quote = FALSE,
      row.names = FALSE,
      col.names = FALSE,
      sep = ""
    )
    utils::write.table(
      df,
      filename,
      quote = FALSE,
      row.names = FALSE,
      col.names = TRUE,
      sep = ",",
      append = TRUE
    )
  } else{
    return(df)
  }
}