#' Function to list all site meta-data
#' 
#' The site list can be helpful in determining which time series to download
#' using `download_phenocam()`. The site list also includes meta-data 
#' concerning plant functional types, general climatological
#' conditions such as mean annual temperature or geographic location.
#' 
#' @param out_dir output directory (default = tempdir())
#' @param internal TRUE or FALSE (default = TRUE)
#' @return A data frame with meta-data for all available sites.
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
  
  # download json data using httr
  error = try(httr::content(httr::GET(url = "https://phenocam.sr.unh.edu/webcam/network/siteinfo/",
                                      httr::timeout(30)),
                            "text",
                            encoding = "UTF-8"))
  
  if (inherits(error, "try-error")){
    stop("Download of ROI list failed, timeout or server error...")
  }
  
  # row bind the json list and replace
  # NULL values with NA
  meta_data = jsonlite::fromJSON(error)
  
  # output according to parameters
  if(internal){
    return(meta_data)
  } else {
    utils::write.table(meta_data,
                       file.path(out_dir, "site_meta_data.csv"),
                       col.names = TRUE,
                       row.names = FALSE,
                       quote = FALSE) 
  }
}