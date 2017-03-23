#' Function to download PhenoCam time series based upon:
#' site name, vegetation type, geographic bounding box.
#' You can optionally merge them with Daymet climate data
#' when located within the Daymet data range.
#'
#' @param site : the site name, as mentioned on the PhenoCam web page.
#' @param vegetation : vegeation type (DB, EN, ... default = ALL)
#' @param frequency : frequency of the time series product (1,3, "raw")
#' @param roi_id: the id of the ROI to download (default = ALL)
#' @param top_lef: latitude, longitude tupple as a vector c(45.5, -80)
#' @param bottom_right: latitude, longitude tupple as a vector c(45.5, -80)
#' @param smooth: smooth data (TRUE / FALSE, default is TRUE)
#' @param daymet : TRUE or FALSE, merges the daymet data
#' @param trim_daymet: TRUE or FALSE, trims data to match PhenoCam data
#' @param outlier_detection: TRUE or FALSE, detect outliers
#' @param phenophase: TRUE or FALSE, calculate transition dates
#' @keywords PhenoCam, Daymet, climate data, modelling
#' @export
#' @examples
#' 
#' \dontrun{
#' # download the first ROI time series for the Harvard PhenoCam site
#' # and an aggregation frequency of 3-days.
#' download.phenocam(site = "harvard",
#'                   vegetation = "DB",
#'                   roi_id = 1,
#'                   frequency = 3)
#'                   
#' # download all Harvard Forest deciduous broadleaf sites using
#' # geographic constraints.
#' download.phenocam(vegetation = "DB",
#'                   roi_id  =1,
#'                   frequency = 3,
#'                   top_left = c(42.545657, -72.197524),
#'                   bottom_right = c(42.527079, -72.158128))
#' }

download.phenocam = function(site="bartlett",
                             vegetation=NULL,
                             frequency="3",
                             roi_id=NULL,
                             top_left= NULL,
                             bottom_right = NULL,
                             outlier_detection=TRUE,
                             smooth=TRUE,
                             daymet=FALSE,
                             trim_daymet=TRUE,
                             phenophase=FALSE,
                             out_dir=getwd()){
  
  # get site listing
  site.list = jsonlite::fromJSON("https://phenocam.sr.unh.edu/webcam/roi/roilistinfo/")
  
  # is there a site name?
  # this excludes any geographic constraints
  
  if (!is.null(site)){
    
    if (is.null(vegetation)){ 
      loc = grep(site,site.list$site)
    }else{
      
      if (!is.null(roi_id)){
        
        # list only particular vegetation types and rois
        loc = which( grepl(site,site.list$site) & site.list$veg_type %in% vegetation &
                      site.list$roi_id_number %in% roi_id)
        
      }else{
        
        # list only vegetation types for all rois
        loc = which( grepl(site,site.list$site) & site.list$veg_type %in% vegetation)
        
      }
    }
    
  }else{
    
    # if there is no site name, use geographic boundaries 
    # (by default all rois are returned)
    
    if (is.null(vegetation) | vegetation == "all"){
      # list all data within a geographic region
      loc = which(site.list$lat < top_left[1] & site.list$lat > bottom_right[1] &
                   site.list$lon > top_left[2] & site.list$lon < bottom_right[2])
    }else{
      # list only a particular vegetation type within a geographic region
      loc = which(site.list$lat < top_left[1] & site.list$lat > bottom_right[1] &
                   site.list$lon > top_left[2] & site.list$lon < bottom_right[2] &
                    site.list$veg_type %in% vegetation)
    }
  }
  
  # if no valid files are detected stop any download attempts
  if (length(loc)==0){
    stop("No files are available for download! \n",call.=TRUE)
  }
  
  # subset the site list
  site.list = site.list[loc,]
  
  # cycle through the selection
  for (i in 1:dim(site.list)[1]){
    
    # create server string, the location of the site data
    if (frequency == "raw"){
      data_location=sprintf("https://phenocam.sr.unh.edu/data/archive/%s/ROI",site.list$site[i])
      filename = sprintf("%s_%s_%04d_timeseries.csv",site.list$site[i],site.list$veg_type[i],site.list$roi_id_number[i])
    } else {
      data_location=sprintf("https://phenocam.sr.unh.edu/data/archive/%s/ROI",site.list$site[i])
      filename = sprintf("%s_%s_%04d_%sday.csv",site.list$site[i],site.list$veg_type[i],site.list$roi_id_number[i],frequency)
    }
    
    # formulate output file location
    output_filename = sprintf("%s/%s",out_dir,filename)
    
    # download data + feedback
    url = sprintf("%s/%s",data_location,filename)
    cat(sprintf("Downloading: %s/%s\n",data_location,filename))
    status = try(curl::curl_download(url,output_filename,quiet=TRUE),silent=TRUE)
    
    # skip if download failed  
    if (inherits(status,"try-error")){
      
      warning(sprintf("failed to download: %s",filename))
      next
      
    } else {
      
      # don't do any post-procesing on raw data
      if (frequency == "raw"){
        next
      }
      
      # if the frequency is 3 expand the table to
      # daily values for easier processing afterwards
      if (frequency == "3"){
        # feedback
        cat("Expanding data set to 1-day frequency! \n")
        
        # expand the time series
        expand.phenocam(output_filename)
      }
      
      # remove outliers (overwrites original file)
      if (outlier_detection==TRUE | outlier_detection == "true" | outlier_detection == "T"){
        # feedback
        cat("Flagging outliers! \n")
        
        # detect outliers
        status=try(detect.outliers(output_filename),silent=TRUE)
        
        # trap errors
        if(inherits(status,"try-error")){
          cat("--failed \n")
        }
      }
      
      # Smooth data
      if (smooth==TRUE | smooth == "true" | smooth == "T"){
        # feedback
        cat("Smoothing time series! \n")
        
        # smooth time series
        status=try(smooth.ts(output_filename),silent=TRUE)
        
        # trap errors
        if(inherits(status,"try-error")){
          cat("--failed \n")
        }
      }
      
      # merge with daymet
      if (daymet==TRUE | daymet == "true" | daymet == "T"){
        # feedback
        cat("Merging Daymet Data! \n")
        
        # merge daymet data into the time series file
        status=try(merge.daymet(output_filename,trim_daymet=trim_daymet),silent=TRUE)
        
        # trap errors
        if(inherits(status,"try-error")){
          cat("--failed merging daymet data, check package \n")
        }
      }
      
    }
  }
}