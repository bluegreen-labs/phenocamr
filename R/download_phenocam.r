#' Function to download and post-process PhenoCam time series
#' 
#' This is a wrapper around most of all the other functions.
#' It downloads a time series and extract relevant phenological transition
#' dates or phenophases.
#'
#' @param site the site name, as mentioned on the PhenoCam web page expressed
#' as a regular expression ("harvard$" == exact match)
#' @param veg_type vegetation type (DB, EN, ... default = ALL)
#' @param frequency frequency of the time series product (1, 3, "roistats")
#' @param roi_id the id of the ROI to download (default = ALL)
#' @param smooth smooth data (logical, default is \code{TRUE})
#' @param daymet TRUE or FALSE, merges the daymet data
#' @param contract contract 3-day data (logical, default is \code{TRUE})
#' @param trim_daymet TRUE or FALSE, trims data to match PhenoCam data
#' @param outlier_detection TRUE or FALSE, detect outliers
#' @param trim year (numeric) to which to constrain the output (default = \code{NULL})
#' @param phenophase logical, calculate transition dates (default = \code{FALSE})
#' @param out_dir output directory where to store downloaded data 
#' (default = tempdir())
#' @return Downloaded files in out_dir of requested time series products, as well
#' as derived phenophase estimates based upon these time series.
#' @keywords PhenoCam, Daymet, climate data, modelling, post-processing
#' @export
#' @examples
#'
#' \donttest{
#' # download the first ROI time series for the Harvard PhenoCam site
#' # at an aggregation frequency of 3-days.
#' download_phenocam(site = "harvard$",
#'                   veg_type = "DB",
#'                   roi_id = "1",
#'                   frequency = "3")
#'  
#' # read phenocam data into phenocamr data structure                  
#' df <- read_phenocam(paste0(tempdir(),"/harvard_DB_0001_3day.csv"))
#'                   
#' }

download_phenocam = function(site = "harvard$",
                             veg_type = NULL,
                             frequency = "3",
                             roi_id = NULL,
                             outlier_detection = TRUE,
                             smooth = TRUE,
                             contract = FALSE,
                             daymet = FALSE,
                             trim_daymet = TRUE,
                             trim = NULL,
                             phenophase = FALSE,
                             out_dir = tempdir()) {

  # sanity check on frequency values
  if (!any(frequency %in% c("1","3","roistats"))){
    stop("No correct frequency provided...")
  }
  
  # get site listing
  site_list = jsonlite::fromJSON("https://phenocam.sr.unh.edu/webcam/roi/roilistinfo/")

  # is there a site name?
  # this excludes any geographic constraints

  if (!is.null(site)){
    if (is.null(veg_type)){
      loc = grep(site, site_list$site)
    }else{

      if (!is.null(roi_id)){

        # list only particular vegetation types and rois
        loc = which( grepl(site,site_list$site) & site_list$veg_type %in% veg_type &
                      site_list$roi_id_number %in% roi_id)

      }else{

        # list only vegetation types for all rois
        loc = which( grepl(site,site_list$site) & site_list$veg_type %in% veg_type)

      }
    }

  }else{
    stop("No site selected, at a minimum provide a site name")
  }

  # if no valid files are detected stop any download attempts
  if (length(loc) == 0){
    stop("No files are available for download! \n",call.=TRUE)
  }

  # subset the site list
  site_list = site_list[loc,]

  # cycle through the selection
  for (i in 1:dim(site_list)[1]){

    # create server string, the location of the site data
    if (frequency == "roistats"){
      data_location=sprintf("https://phenocam.sr.unh.edu/data/archive/%s/ROI",
                            site_list$site[i])
      filename = sprintf("%s_%s_%04d_roistats.csv",
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
    output_filename = sprintf("%s/%s",
                              out_dir,
                              filename)

    # download data + feedback
    cat(sprintf("Downloading: %s\n", filename))
    
    url = sprintf("%s/%s",data_location, filename)
    status = try(curl::curl_download(url,
                                     output_filename,
                                     quiet = TRUE),
                 silent = TRUE)

    # skip if download failed
    if (inherits(status,"try-error")){
      warning(sprintf("failed to download: %s", filename))
      next
    } else {
      
      # don't do any post-procesing on raw data
      if (frequency == "roistats"){
        next
      }

      # read in data using read_phenocam to process all in memory
      df = read_phenocam(output_filename)
      
      # always expand the time series to get maximal
      # phenophase date resolution as well as additional
      # padding around the ends - 90 days
      if(!is.null(trim) & is.numeric(trim)){
        df = expand_phenocam(df, truncate = trim)
      } else {
        df = expand_phenocam(df)
      }
      
      # remove outliers (overwrites original file)
      if (outlier_detection == TRUE){
        
        # feedback
        cat("Flagging outliers! \n")

        # detect outliers
        df = try(suppressWarnings(detect_outliers(df)),
                                      silent = TRUE)

        # trap errors
        if(inherits(df,"try-error")){
          cat("--failed \n")
        }
      }
      
      # Smooth data
      if (smooth == TRUE){
        # feedback
        cat("Smoothing time series! \n")
        
        # smooth time series
        df = try(suppressWarnings(smooth_ts(df)),
                                      silent = TRUE)

        # trap errors
        if(inherits(df,"try-error")){
          warning("smoothing failed...")
        }
      }

      # Output transition dates
      if (phenophase == TRUE){
        
        # feedback
        cat("Estimating transition dates! \n")

        # smooth time series
        phenophase_check = try(suppressWarnings(phenophases(data = df,
                                 out_dir = out_dir,
                                 internal = FALSE)),
                     silent = TRUE)

        # trap errors
        if(inherits(phenophase_check, "try-error")){
          warning("estimating transition dates failed...")
        }
      }

      # merge with daymet
      if (daymet == TRUE){
        
        # feedback
        cat("Merging Daymet Data! \n")

        # merge daymet data into the time series file
        df = try(merge_daymet(df,
                              trim = trim_daymet),
                     silent = TRUE)

        # trap errors
        if(inherits(df,"try-error")){
          warning("merging daymet data failed...")
        }
      }
      
      # contract datasets if so desired
      if (contract == TRUE & df$frequency == "3day"){
        
        # feedback
        cat("Contracting Data! \n")
        
        # merge daymet data into the time series file
        df = try(contract_phenocam(df),
                 silent = TRUE)
        
        # trap errors
        if(inherits(df,"try-error")){
          warning("contracting data failed...")
        }
      }
      
      # finally output all the data to file
      write_phenocam(df, out_dir = out_dir)
    }
  }
}
