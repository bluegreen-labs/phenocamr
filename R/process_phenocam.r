#' Function to post-process PhenoCam time series
#' 
#' Wrapper around other more basic funtions
#'
#' @param file 1 or 3-day PhenoCam time series file path
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
#' @param snow_flag integrate snow flags?
#' @param snow_weight weight given to snow flagged values, by default these are
#' not down weighted in smoothing (default = 1)
#' @param span customized span list or variable, default is NULL
#' @param save_gcc90 whether to save the plot output for gcc_90 timeseries (as a test)
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
  daymet = FALSE,
  trim_daymet = TRUE,
  trim = NULL,
  phenophase = FALSE,
  out_dir = tempdir(),
  internal = FALSE,
  snow_flag = FALSE, 
  snow_weight = 1,
  save_gcc90 = FALSE,
  span = NULL
){
  
  # check file
  if(missing(file)){
    stop("No file path provided")
  }
  
  # read in data using read_phenocam to process all in memory
  df = read_phenocam(file)
  
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
    df.tmp = try(suppressWarnings(detect_outliers(df, 
                                                  snow_flag = snow_flag)),
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
    df.tmp = try(suppressWarnings(smooth_ts(df, 
                                            span = span,
                                            save_gcc90 = save_gcc90,
                                            snow_weight = snow_weight)),
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
    phenophase_check = try(suppressWarnings(
      phenophases(data = df,
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
      dt <- df.tmp
    }
  }
  
  # finally output all the data to file or workspace
  if(!internal){
    write_phenocam(df, out_dir = out_dir)
  }else {
    return(df)    
  }
}