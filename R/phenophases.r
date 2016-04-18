#' Calculates phenophases for all 'seasons' in a PhenoCam
#' time series. This routine combines a forward and backward
#' run of transition.dates() to calculate all phenophases.
#' @param df: a PhenoCam data file or data frame
#' @param plot: provide a basic graph of the data and estimated
#' dates
#' @param pdf: output plot as pdf in current working directory
#' @keywords PhenoCam, transition dates, phenology, time series
#' @export
#' @examples
#' # with defaults, outputting a nested list of phenophases dates
#' # where location [[1]] holds the greenup dates and location
#' [[2]] the greendown dates
#' 
#' my_dates <- phenophases(df,plot=TRUE)
#' 
#' # dates need to be converted to standard notation using
#' as.Date(my_dates)

phenophases <- function(df,plot=TRUE,pdf=FALSE){
  
  x = smooth.ts(df)
  
  # calculate start of season values (greenup)
  sos = transition.dates(x,reverse=FALSE)
  
  # calculate end of season values (senescence)
  eos = transition.dates(x,reverse=TRUE)

  # return dates as a list
  return(list(sos,eos))
}