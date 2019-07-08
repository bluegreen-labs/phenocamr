#' Function to post-process PhenoCam time series
#' 
#' Wrapper around other more basic funtions, in order to generate phenocam
#' data products.
#'
#' @param file 1 or 3-day PhenoCam time series file path
#' @param smooth smooth data (logical, default is \code{TRUE})
#' @param contract contract 3-day data upon output
#' (logical, default is \code{TRUE})
#' @param expand expand 3-day data upon input 
#' (logical, default is \code{TRUE})
#' @param outlier_detection TRUE or FALSE, detect outliers
#' @param truncate year (numeric) to which to constrain the output
#' @param phenophase logical, calculate transition dates (default = \code{FALSE})
#' @param out_dir output directory where to store downloaded data 
#' (default = tempdir())
#' @param internal allow for the data element to be returned to the workspace
#' @param snowflag integrate snow flags?
#' @param penalty how sensitive is the change point algorithm, lower is more
#' sensitve (< 1, default = 0.5)
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
#'                   roi_id = "1000",
#'                   frequency = "3")
#'  
#' # read phenocam data into phenocamr data structure                  
#' df <- process_phenocam(file.path(tempdir(),"harvard_DB_1000_3day.csv"))
#'                   
#' }

process_phenocam <- function(
  file,
  outlier_detection = TRUE,
  smooth = TRUE,
  contract = FALSE,
  expand = TRUE,
  truncate,
  phenophase = TRUE,
  snow_flag = FALSE,
  penalty = 0.5,
  out_dir = tempdir(),
  internal = FALSE,
  ...
){
  
  # check file
  if(missing(file)){
    stop("No file path provided")
  }
  
  # read in data using read_phenocam to process all in memory
  message(paste0("Processing file: ", file))
  df <- read_phenocam(file)
  
  # By default always contract files to start the processing
  # to avoid including previously padded areas
  df <- contract_phenocam(df,
                          no_padding = TRUE)
  
  # Expand the time series to get maximal
  # phenophase date resolution as well as additional
  # padding around the ends - 90 days
  if(expand){
    if(!missing(truncate) & is.numeric(truncate)){
      df <- expand_phenocam(df,
                           truncate = truncate)
    } else {
      df <- expand_phenocam(df)
    }
  }
  
  # remove outliers (overwrites original file)
  if (outlier_detection){
    
    # feedback
    message("-- Flagging outliers!")
    
    # detect outliers
    df <- try(suppressWarnings(
      detect_outliers(df)),
             silent = TRUE)

    # trap errors
    if(inherits(df, "try-error")){
      warning("outlier detection failed...")
    }
  }
  
  # Smooth data
  if (smooth){
    # feedback
    message("-- Smoothing time series!")
    
    # smooth time series, force processing as to redo any
    # calculations should smoothed data be provided
    df <- try(suppressWarnings(
      smooth_ts(df,
                force = TRUE)),
             silent = TRUE)
    
    # trap errors
    if(inherits(df, "try-error")){
      warning("smoothing failed...")
    }
  }
  
  # Output transition dates
  if (phenophase){
    
    # feedback
    message("-- Estimating transition dates!")
    
    # calculate phenophases
    phenophase_check <- try(suppressWarnings(
      phenophases(
        data = df,
        out_dir = out_dir,
        internal = FALSE,
        penalty = penalty,
        ...)
      ), silent = TRUE)
    
    # trap errors
    if(inherits(phenophase_check, "try-error")){
      warning("estimating transition dates failed...")
    }
  }
  
  # contract datasets if so desired
  if (contract & df$frequency == "3day"){
    
    # feedback
    message("-- Contracting Data!")
    
    # merge daymet data into the time series file
    df = try(contract_phenocam(df),
             silent = TRUE)
    
    # trap errors
    if(inherits(df,"try-error")){
      warning("contracting data failed...")
    }
  }
  
  # finally output all the data to file or workspace
  if(!internal){
    write_phenocam(df, out_dir = out_dir)
  }else {
    return(df)
  }
}