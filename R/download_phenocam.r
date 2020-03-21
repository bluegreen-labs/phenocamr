#' Downloads PhenoCam time series
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
#' @param internal allow for the data element to be returned to the workspace
#' @return Downloaded files in out_dir of requested time series products, as well
#' as derived phenophase estimates based upon these time series.
#' @keywords PhenoCam, Daymet, climate data, modelling, post-processing
#' @export
#' @examples
#'
#' \dontrun{
#' # download the first ROI time series for the Harvard PhenoCam site
#' # at an aggregation frequency of 3-days.
#' download_phenocam(site = "harvard$",
#'                   veg_type = "DB",
#'                   roi_id = "1000",
#'                   frequency = "3")
#'  
#' # read phenocam data into phenocamr data structure                  
#' df <- read_phenocam(file.path(tempdir(),"harvard_DB_1000_3day.csv"))
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
                             out_dir = tempdir(),
                             internal = FALSE){

  # sanity check on frequency values
  if (!any(frequency %in% c("1","3","roistats"))){
    stop("No correct frequency provided...")
  }
  
  # get roi site listing
  site_list <- list_rois()
  
  # check for server timeout
  if (inherits(site_list,"try-error")){
    stop("Downloading ROI list failed...")
  }

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
  site_list <- site_list[loc,]

  # cycle through the selection
  for (i in 1:dim(site_list)[1]){

    # build url
    url_info <- server_archive(frequency = frequency,
                               site = site_list$site[i],
                               veg_type = site_list$veg_type[i],
                               roi_id_number = site_list$roi_id_number[i])
    filename <- url_info$filename
    data_location <- url_info$data_location

    # formulate output file location
    output_filename = file.path(out_dir, filename)

    # download data + feedback
    message(sprintf("Downloading: %s", filename))
    
    # try to download the data
    error = try(httr::GET(
      url = sprintf("%s/%s",data_location, filename),
      httr::write_disk(path = output_filename, overwrite = TRUE)))
    
    # trap errors on download, return a general error statement
    # with the most common causes
    if (inherits(error, "try-error")){
      try(file.remove(output_filename))
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
      if (outlier_detection){
        
        # feedback
        message("-- Flagging outliers!")

        # detect outliers
        df.tmp = try(suppressWarnings(detect_outliers(df)),
                                      silent = TRUE)

        # trap errors
        if(inherits(df.tmp, "try-error")){
          warning("outlier detection failed...")
        }else{
          df <- df.tmp
        }
      }
      
      # Smooth data
      if (smooth){
        # feedback
        message("-- Smoothing time series!")
        
        # smooth time series
        df.tmp = try(suppressWarnings(smooth_ts(df)),
                                      silent = TRUE)

        # trap errors
        if(inherits(df.tmp,"try-error")){
          warning("smoothing failed...")
        }else{
          df <- df.tmp
        }
      }

      # Output transition dates
      if (phenophase){
        
        # feedback
        message("-- Estimating transition dates!")

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
      if (daymet){
        
        # feedback
        message("-- Merging Daymet Data!")

        # merge daymet data into the time series file
        df.tmp = try(merge_daymet(df,
                              trim = trim_daymet),
                     silent = TRUE)

        # trap errors
        if(inherits(df.tmp,"try-error")){
          warning("merging daymet data failed...")
        }else{
          df <- df.tmp
        }
      }
      
      # contract datasets if so desired
      if (contract & df$frequency == "3day"){
        
        # feedback
        message("-- Contracting Data!")
        
        # merge daymet data into the time series file
        df.tmp = try(contract_phenocam(df),
                 silent = TRUE)
        
        # trap errors
        if(inherits(df.tmp,"try-error")){
          warning("contracting data failed...")
        }else{
          df <- df.tmp
        }
      }
      
      # finally output all the data to file or workspace
      if(!internal){
        write_phenocam(df, out_dir = out_dir)
      }else {
        return(df)    
      }
    }
  }
}
