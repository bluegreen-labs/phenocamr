#' Contracts the file from 1-day to a 3-day time step
#' reverting the expand.phenocam() routine in order
#' to save space and generate files as outlined in
#' the data paper. This routine is used as a post-production
#' measure, and not used in general manipulation
#' of the data as even 1-day files are sufficiently
#' small.
#' 
#' @param file: a phenocam data file with a 3 day time step
#' @keywords time series, smoothing, phenocam
#' @export
#' @examples
#' # overwrites the original file, decreasing
#' # its file size and reducing the time step to
#' # once 3-day intervals
#' contract.phenocam(file="filename")

contract.phenocam = function(filename){
  
  # check validaty of the input
  if(is.data.frame(filename)){
    stop("not a PhenoCam data file")
  }
  
  # suppress warnings as it throws unnecessary warnings
  # messing up the feedback to the CLI
  header = try(readLines(filename,n=22),silent=TRUE)
  
  # directly read data from the server into data.table
  data = read.table(filename,header=TRUE,sep=",")
  
  # drop the lines which should be empty
  loc = seq(2,366,3)
  data = data[which(data$doy %in% loc),]
  
  # replace all -9999 values with NA
  # for consistency
  df[df == -9999] = NA
  
  # writing the final data frame to file, retaining the original header
  write.table(header,filename,quote=FALSE,row.names=FALSE,col.names=FALSE,sep="")
  write.table(data,filename,quote=FALSE,row.names=FALSE,col.names=TRUE,sep=",",append = TRUE)
}