#' Function to list all site meta-data
#' 
#' The site list can be helpful in determining which time series to download
#' using `download_phenocam()`. The site list also includes meta-data 
#' concerning plant functional types, general climatological
#' conditions such as mean annual temperature or geographic location.
#' 
#' @param out_dir output directory (default = tempdir())
#' @param internal TRUE or FALSE (default = TRUE)
#' @keywords PhenoCam, meta-data
#' @export
#' @examples
#'
#' \donttest{
#' # download the site meta-data
#' df <- list_sites()
#' }

list_sites <- function(out_dir = tempdir(),
                       internal = TRUE){
  
  # donwload the data
  meta_data <- jsonlite::fromJSON("https://phenocam.sr.unh.edu/webcam/network/siteinfo/")
  
  # output according to parameters
  if(internal){
    return(meta_data)
  } else {
    utils::write.table(meta_data,
                       paste0(tempdir(),"/site_meta_data.csv"),
                       col.names = TRUE,
                       row.names = FALSE,
                       quote = FALSE) 
  }
}