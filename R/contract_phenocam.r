#' Contracts the file from 1-day to a 3-day time step
#' 
#' Reverts the `expand_phenocam()` function in order
#' to save space and generate files as outlined in
#' the cited data paper. This routine is used as a 
#' post-production measure.
#'
#' @param data a phenocam data file with a 3 day time step
#' @param internal return a data structure if given a file on disk
#' (\code{TRUE} / \code{FALSE} = default) 
#' @param no_padding allow for padding to REMAIN or not  
#' (\code{TRUE} / \code{FALSE} = default)
#' @param out_dir output directory where to store data (default = tempdir())
#' @return A contracted PhenoCam 3-day time series to its original 3-day time
#' step (if provided at a 1-day interval).
#' @keywords time series, phenocam, post-processing
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

contract_phenocam = function(data,
                             internal = TRUE,
                             no_padding = FALSE,
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
  
  # sanity check, only contract 3-day data
  if(data$frequency == "1day"){
    stop("This is a 1-day file, will not be contracted to 3-day frequency ")
  }
  
  # split out data
  df = data$data
  
  # drop the lines which should be empty or have
  # no image count associated (removes the padding)
  if (!no_padding){
    loc = seq(2, 366, 3)
    df = df[which(as.numeric(df$doy) %in% loc), ]
  } else {
    df = df[which(!is.na(df$image_count)),]
  }
  
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