#' Function to download PhenoCam time series based upon:
#' site name, vegetation type, geographic bounding box.
#' You can optionally merge them with Daymet climate data
#' when located within the Daymet data range.
#'
#' @param site : the site name, as mentioned on the PhenoCam web page.
#' @param vegetation : vegeation type (DB, EN, ... default = ALL)
#' @param frequency : frequency of the PhenoCam time series product, in days (1,3, "raw" are allowed)
#' @param roi_id: the id of the ROI to download, if not specified download all
#' @param top_lef: latitude, longitude tupple as a vector c(45.5, -80), requires bottom_right
#' @param bottom_right: latitude, longitude tupple as a vector c(45.5, -80), requires top_left
#' @param smooth: smooth data for future plotting (TRUE / FALSE, default is TRUE)
#' @param daymet : TRUE or FALSE, if true merges the daymet data at the location (only for frequency 1 and 3)
#' @param trim_daymet: TRUE or FALSE, if TRUE only download Daymet data for the dates matching the Gcc data. 
#' If FALSE, I provide the whole Daymet series (for modelling spin-up purposes).
#' @param outlier_detection: TRUE or FALSE, detect outliers
#' @param phenophase: TRUE or FALSE, calculate threshold based phenological transition dates (this will generate a non standard product)
#' @keywords PhenoCam, Daymet, climate data, modelling
#' @export
#' @examples
#' 
#' # download the first ROI time series for the Harvard PhenoCam site
#' # and an aggregation frequency of 3-days.
#' # download.phenocam(site="harvard",
#' #                   vegetation="DB",
#' #                   roi_id=1,
#' #                   frequency=3)
#'                   
#' # download all Harvard Forest deciduous broadleaf sites using
#' # geographic constraints.
#' # download.phenocam(vegetation="DB",
#' #                   roi_id=1,
#' #                   frequency=3,
#' #                   top_left=c(42.545657, -72.197524),
#' #                   bottom_right=c(42.527079, -72.158128))

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
      loc = grep(sprintf("%s",site),site.list$site)
    }else{
      if (!is.null(roi_id)){
        
        # list only particular vegetation types and rois
        loc = which( grep(sprintf("%s",site),site.list$site) && site.list$veg_type %in% vegetation &&
                      site.list$roi_id_number %in% roi_id)
        
      }else{
        
        # list only vegetation types for all rois
        loc = which( grep(sprintf("%s",site),site.list$site) && site.list$veg_type %in% vegetation)
        
      }
    }
    
  }else{
    
    # if there is no site name, use geographic boundaries 
    # (by default all rois are returned)
    
    if (is.null(vegetation) | vegetation == "all"){
      # list all data within a geographic region
      loc = which(site.list$lat < top_left[1] && site.list$lat > bottom_right[1] &&
                   site.list$lon > top_left[2] && site.list$lon < bottom_right[2])
    }else{
      # list only a particular vegetation type within a geographic region
      loc = which(site.list$lat < top_left[1] && site.list$lat > bottom_right[1] &&
                   site.list$lon > top_left[2] && site.list$lon < bottom_right[2] &&
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
    if (frequency == "raw"){
      
      data_location=sprintf("https://phenocam.sr.unh.edu/data/archive/%s/ROI",site.list$site[i])
      filename = sprintf("%s_%s_%04d_timeseries.csv",site.list$site[i],site.list$veg_type[i],site.list$roi_id_number[i])
      
      # feedback
      cat(sprintf("Downloading: %s/%s\n",data_location,filename))
      try(curl::curl_download(sprintf("%s/%s",data_location,filename),sprintf("%s/%s",out_dir,filename),quiet=TRUE),silent=TRUE)
      
      if (inherits(status,"try-error")){
        warning(sprintf("failed to download: %s",filename))
        next
      }
      
    }else{
      
      # create server string, the location of the site data
      data_location=sprintf("https://phenocam.sr.unh.edu/data/archive/%s/ROI",site.list$site[i])
      filename = sprintf("%s_%s_%04d_%sday.csv",site.list$site[i],site.list$veg_type[i],site.list$roi_id_number[i],frequency)
      output_filename = sprintf("%s/%s",out_dir,filename)
      
      # feedback
      cat(sprintf("Downloading: %s/%s\n",data_location,filename))
      
      # download data
      url = sprintf("%s/%s",data_location,filename)
      status = try(curl::curl_download(url,output_filename,quiet=TRUE),silent=TRUE)
      
      if (inherits(status,"try-error")){
        warning(sprintf("failed to download: %s",filename))
        next
      }else{
      
        # if the frequency is 3 expand the table to
        # daily values for easier processing afterwards
        # at a slight size increase cost
        if (frequency == "3"){
          # feedback
          cat("Expanding data set to 1-day frequency! \n")
          expand.phenocam(output_filename)
        }
        
        # remove outliers (overwrites original file)
        if (outlier_detection==TRUE | outlier_detection == "true" | outlier_detection == "T"){
          # feedback
          cat("Flagging outliers! \n")
          status=try(detect.outliers(output_filename),silent=TRUE)
          if(inherits(status,"try-error")){
            cat("--failed \n")
          }
        }
        
        # Smooth data on download? Speeds up processing afterwards
        # does increase the file size
        if (smooth==TRUE | smooth == "true" | smooth == "T"){
          # feedback
          cat("Smoothing time series! \n")
          status=try(smooth.ts(output_filename),silent=TRUE)
          if(inherits(status,"try-error")){
            cat("--failed \n")
          }
        }
        
        # merge with daymet
        if (daymet==TRUE | daymet == "true" | daymet == "T"){
          # feedback
          cat("Merging Daymet Data! \n")
          status=try(merge.daymet(output_filename,trim_daymet=trim_daymet),silent=TRUE)
          if(inherits(status,"try-error")){
            cat("--failed merging daymet data, check package \n")
          }
        }
      }
    }
  }
}