#' Function to list all site regions-of-interst (ROIs)
#' 
#' The ROI list can be helpful in determining which time series to download
#' using `download_phenocam()`.
#' 
#' @param out_dir output directory (default = tempdir())
#' @param internal TRUE or FALSE (default = TRUE)
#' @return A data frame with ROIs for all available cameras
#' @keywords PhenoCam, meta-data
#' @export
#' @examples
#'
#' \donttest{
#' # download the site meta-data
#' df <- list_rois()
#' }

list_rois <- function(out_dir = tempdir(),
                       internal = TRUE){
  
  # download json data using httr
  error <- httr::GET(url = server_rois(),
                        httr::timeout(30))
  
  if (httr::http_error(error)){
    stop("Download of ROI list failed, timeout or server error...")
  }
  
  # row bind the json list and replace
  # NULL values with NA
  roi_data <- jsonlite::fromJSON(
    httr::content(error, "text", encoding = "UTF-8")
    )

  # output according to parameters
  if(internal){
    return(roi_data)
  } else {
   utils::write.table(roi_data,
                      file.path(out_dir, "roi_data.csv"),
                      col.names = TRUE,
                      row.names = FALSE,
                      quote = FALSE) 
  }
}
