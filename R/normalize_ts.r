#' Normalize PhenoCam time series
#' 
#' Normalize PhenoCam data between 0-1 to to standardize
#' further processing, independent of the relative amplitude
#' of the time series (works on vectors not data frames).
#' For internal use only.
#' 
#' @param df a PhenoCam data frame
#' @param percentile percentile value to interprete
#' @return A normalized PhenoCam time series.
#' @keywords time-series smoothing phenocam
#' @export
#' @examples
#' 
#' # Internal function only, should not be used stand-alone.
#' # As such no documentation is provided.

normalize_ts <- function(df, percentile = 90){
  
  if (!is.data.frame(df)){
    stop("not a data.frame")
  }
  
  smooth = df[,which(colnames(df) == sprintf("smooth_gcc_%s",percentile))]
  ci = df[,which(colnames(df) == sprintf("smooth_ci_gcc_%s",percentile))]
  upper = smooth + ci
  lower = smooth - ci
  
  # find range
  max_val = max(c(smooth, upper, lower), na.rm=T)
  min_val = min(c(smooth, upper, lower), na.rm=T)
  
  # normalize
  smooth  = (smooth - min_val)/(max_val - min_val)
  upper  = (upper - min_val)/(max_val - min_val)
  lower  = (lower - min_val)/(max_val - min_val)
  
  # return data
  return(data.frame(smooth, upper, lower))
}
