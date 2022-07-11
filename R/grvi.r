#' Calculate green-red vegetation index (GRVI)
#'
#' The GRVI is defined as the normalized ratio between the red and green channel
#' of a RGB image or digital number triplet. However, the blue channel can be
#' used as well using a weighting factor. As such a paramter vector is provided
#' so different channels / DN can be weighted separately.
#'   
#' @param data a PhenoCam data file or data frame (when using a file provide a
#' full path if not in the current working directory)
#' @param par grvi parameters (digital number weights)
#' @param internal return a data structure if given a file on disk
#' (\code{TRUE} / \code{FALSE} = default)
#' @param out_dir output directory where to store data
#' @return Inserts a GRVI data column into the provided PhenoCam
#' data structure or file.
#' @export
#' @examples
#' 
#' \dontrun{
#' # with defaults, outputting a data frame
#' # with smoothed values, overwriting the original
#' 
#' # download demo data
#' download_phenocam(site = "harvard$",
#'                   veg_type = "DB",
#'                   roi_id = "1000",
#'                   frequency = "3")
#' 
#' # calculate and append the GRVI for a file (overwrites the original)
#' grvi(file.path(tempdir(),"harvard_DB_1000_3day.csv"))
#' 
#' # as all functions this also works on a PhenoCam data structure
#' df <- read_phenocam(file.path(tempdir(),"harvard_DB_1000_3day.csv"))
#' df <- grvi(df, par = c(1, 1, 0))
#' }

grvi = function(data,
                par=c(1, 1, 1),
                internal = TRUE,
                out_dir = tempdir()) {
  
  # read parameters into readable formats
  a = par[1]
  b = par[2]
  c = par[3]
  
  # if the data is not a data frame, load
  # the file (assuming it is a phenocam)
  # summary file, otherwise rename the
  # input data to df
  if(!inherits(data, "phenocamr")){
    if(file.exists(data)){
      data = read_phenocam(data)
      on_disk = TRUE
    } else {
      stop("not a valid PhenoCam data frame or file")
    }
  } else {
    on_disk = FALSE
  }
  
  # copy data into data frame to manipulate
  df = data$data
  
  # strip out necessary data into readable variables
  green = df$g_mean
  red = df$r_mean
  blue = df$b_mean
    
  # calculate the GRVI
  df$grvi = (green * a - red * b  - blue * c) /
      (green * a + red * b + blue * c)
  
  # put data back into the data structure
  data$data = df
  
  # write the data to the original data frame or the
  # original file (overwrites the data!!!)
  if(on_disk | !internal ){
    write_phenocam(data, out_dir = out_dir)
  } else {
    # if provided a data frame
    # return the original data frame, with flagged outliers
    class(data) <- "phenocamr"
    return(data)
  }
}