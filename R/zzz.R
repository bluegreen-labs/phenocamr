# server end points
server_lists <- function(){
  "https://phenocam.nau.edu/webcam/network/siteinfo/"
}

server_rois <- function(){
  "https://phenocam.nau.edu/webcam/roi/roilistinfo/"
}

server_archive <- function(frequency, site, veg_type, roi_id_number){
  # create server string, the location of the site data
  if (frequency == "roistats"){
    data_location=sprintf("https://phenocam.nau.edu/data/archive/%s/ROI",
                          site)
    filename = sprintf("%s_%s_%04d_roistats.csv",
                       site,
                       veg_type,
                       roi_id_number)
  } else {
    data_location=sprintf("https://phenocam.nau.edu/data/archive/%s/ROI",
                          site)
    filename = sprintf("%s_%s_%04d_%sday.csv",
                       site,
                       veg_type,
                       roi_id_number,
                       frequency)
  }

  # return data
  return(list(filename = filename, data_location = data_location))  
}

# check if server is reachable
# returns bolean TRUE if so
phenocam_running <- function(url = "http://phenocam.nau.edu"){
  ct <- try(httr::GET(url), silent = TRUE)
  if(inherits(ct,"try-error")){
    return(FALSE)
  }
  if(ct$status_code > 400){
    FALSE  
  } else {
    TRUE
  }
}
