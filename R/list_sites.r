#' Function to list site meta-data
#' 
#' @param out_dir output directory (default = tempdir())
#' @param internal TRUE or FALSE (default = TRUE)
#' @keywords PhenoCam, meta-data
#' @export
#' @examples
#'
#' \dontrun{
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
                       paste0(tempdir(),"site_meta_data.csv"),
                       col.names = TRUE,
                       row.names = FALSE,
                       quote = FALSE) 
  }
  
}