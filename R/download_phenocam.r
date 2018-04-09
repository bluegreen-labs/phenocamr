#' Function to download and post-process PhenoCam time series
#'
#' @param site the site name, as mentioned on the PhenoCam web page expressed
#' as a regular expression ("harvard$" == exact match)
#' @param vegetation vegetation type (DB, EN, ... default = ALL)
#' @param frequency frequency of the time series product (1, 3, "raw")
#' @param roi_id the id of the ROI to download (default = ALL)
#' @param smooth smooth data (TRUE / FALSE, default is TRUE)
#' @param daymet TRUE or FALSE, merges the daymet data
#' @param trim_daymet TRUE or FALSE, trims data to match PhenoCam data
#' @param outlier_detection TRUE or FALSE, detect outliers
#' @param phenophase TRUE or FALSE, calculate transition dates
#' @param out_dir output directory where to store downloaded data 
#' (default = tempdir())
#' @keywords PhenoCam, Daymet, climate data, modelling
#' @export
#' @examples
#'
#' \donttest{
#' # download the first ROI time series for the Harvard PhenoCam site
#' # at an aggregation frequency of 3-days.
#' download_phenocam(site = "harvard$",
#'                   vegetation = "DB",
#'                   roi_id = "1",
#'                   frequency = "3")
#'
#' }

download_phenocam = function(site = "harvard$",
                             vegetation = NULL,
                             frequency = "3",
                             roi_id = NULL,
                             outlier_detection = TRUE,
                             smooth = TRUE,
                             daymet = FALSE,
                             trim_daymet = TRUE,
                             phenophase = FALSE,
                             out_dir = tempdir()) {

  # get site listing
  site_list = jsonlite::fromJSON("https://phenocam.sr.unh.edu/webcam/roi/roilistinfo/")

  # is there a site name?
  # this excludes any geographic constraints

  if (!is.null(site)){
    if (is.null(vegetation)){
      loc = grep(site,site_list$site)
    }else{

      if (!is.null(roi_id)){

        # list only particular vegetation types and rois
        loc = which( grepl(site,site_list$site) & site_list$veg_type %in% vegetation &
                      site_list$roi_id_number %in% roi_id)

      }else{

        # list only vegetation types for all rois
        loc = which( grepl(site,site_list$site) & site_list$veg_type %in% vegetation)

      }
    }

  }else{
    stop("No site selected, at a minimum provide a site name")
  }

  # if no valid files are detected stop any download attempts
  if (length(loc)==0){
    stop("No files are available for download! \n",call.=TRUE)
  }

  # subset the site list
  site_list = site_list[loc,]

  # cycle through the selection
  for (i in 1:dim(site_list)[1]){

    # create server string, the location of the site data
    if (frequency == "raw"){
      data_location=sprintf("https://phenocam.sr.unh.edu/data/archive/%s/ROI",
                            site_list$site[i])
      filename = sprintf("%s_%s_%04d_timeseries.csv",
                         site_list$site[i],
                         site_list$veg_type[i],
                         site_list$roi_id_number[i])
    } else {
      data_location=sprintf("https://phenocam.sr.unh.edu/data/archive/%s/ROI",
                            site_list$site[i])
      filename = sprintf("%s_%s_%04d_%sday.csv",
                         site_list$site[i],
                         site_list$veg_type[i],
                         site_list$roi_id_number[i],
                         frequency)
    }

    # formulate output file location
    output_filename = sprintf("%s/%s",out_dir,filename)

    # download data + feedback
    cat(sprintf("Downloading: %s\n", filename))
    
    url = sprintf("%s/%s",data_location, filename)
    status = try(curl::curl_download(url,
                                     output_filename,
                                     quiet = TRUE),
                 silent=TRUE)

    # skip if download failed
    if (inherits(status,"try-error")){

      warning(sprintf("failed to download: %s",filename))
      next

    } else {

      # don't do any post-procesing on raw data
      if (frequency == "raw"){
        next
      }

      # remove outliers (overwrites original file)
      if (outlier_detection == TRUE |
          tolower(outlier_detection) == "true" |
          tolower(outlier_detection) == "t"){
        
        # feedback
        cat("Flagging outliers! \n")

        # detect outliers
        status = try(suppressWarnings(detect_outliers(output_filename)),
                                      silent = TRUE)

        # trap errors
        if(inherits(status,"try-error")){
          cat("--failed \n")
        }
      }

      # Smooth data
      if (smooth == TRUE |
          tolower(smooth) == "true" |
          tolower(smooth) == "t"){
        # feedback
        cat("Smoothing time series! \n")

        # smooth time series
        status = try(suppressWarnings(smooth_ts(output_filename)),
                                      silent = TRUE)

        # trap errors
        if(inherits(status,"try-error")){
          cat("--failed \n")
        }
      }

      # Output transition dates
      if (phenophase == TRUE |
          tolower(phenophase) == "true" |
          tolower(phenophase) == "t"){
        # feedback
        cat("Estimating transition dates! \n")

        # smooth time series
        status = try(suppressWarnings(phenophases(output_filename,
                                 out_dir = out_dir,
                                 output = TRUE)),
                     silent = TRUE)

        # trap errors
        if(inherits(status,"try-error")){
          cat("--failed \n")
        }
      }

      # merge with daymet
      if (daymet == TRUE |
          tolower(daymet) == "true" |
          tolower(daymet) == "t"){
        
        # feedback
        cat("Merging Daymet Data! \n")
        
        # if the frequency is 3 expand the table to
        # daily values for easier processing afterwards
        if (frequency == "3"){
          # feedback
          cat("-- Expanding data set to 1-day frequency! \n")
          
          # expand the time series
          expand_phenocam(output_filename)
        }

        # merge daymet data into the time series file
        status = try(merge_daymet(output_filename,
                                trim_daymet = trim_daymet),
                     silent=TRUE)

        # trap errors
        if(inherits(status,"try-error")){
          cat("--failed merging daymet data, check package \n")
        }
      }
    }
  }
}
