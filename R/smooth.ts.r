#' Smooths a standard PhenoCam file or data frame using
#' the BCI optimized smoothing parameter
#' @param df: a PhenoCam data file or data frame
#' @keywords time series, smoothing, phenocam
#' @export
#' @examples
#' # with defaults, outputting a data frame
#' # with smoothed values, overwriting the original
#' df <- smooth.ts(df)

smooth.ts <- function(df,force=TRUE){
  
  # load libraries
  require(zoo)
  
  # assign filename
  filename = df
  
  # which columns to process
  metrics = c("r_mean","g_mean","b_mean","gcc_mean","gcc_50","gcc_75","gcc_90","rcc_mean","rcc_50","rcc_75","rcc_90")
  
  # if the data is not a data frame, load
  # the file (assuming it is a phenocam)
  # summary file, otherwise rename the
  # input data to df
  if(!is.data.frame(df)){
    if(file.exists(df)){
      # read data file
      header = try(readLines(filename,n=22),silent=TRUE)
      df = read.table(filename,header=TRUE,sep=",")
    }else{
      stop("not a valid PhenoCam data frame or file")
    }
  }
  
  # if it's a smoothed file, bail unless you want to
  # force the smoothing again
  if (any(grepl("smooth_*",colnames(df))) & force == FALSE ){
    stop("data is already smoothed")
  }
  
  # create convenient date vector
  # (static for all data)
  dates = as.Date(df$date)
  
  # create output matrix
  output = matrix(NA,length(dates),length(metrics)*2+1)
  output = as.data.frame(output)
  column_names = c(sprintf("smooth_%s",metrics),sprintf("smooth_ci_%s",metrics),"int_flag")
  colnames(output) = column_names
  
  # loop over all metrics that need smoothing
  for (i in metrics){
    
    # get the values to use for smoothing
    values = df[,which(colnames(df) == i)]
  
    # flag all outliers as NA
    # if the metric is gcc based
    if (grepl("gcc",i)){
      outliers = df[,which(colnames(df) == sprintf("outlierflag_%s",i))]
      values[outliers==1]=NA
    }

    # smooth input series for plotting
    optim.span = optimal.span(values)
    fit = loess(values ~ as.numeric(dates),span = optim.span)
    fit = predict(fit,as.numeric(dates),se=TRUE)
    
    # grab the smoothed series and the CI
    # set to 0 if no SE is provided
    values_smooth = fit$fit
    
    if (any(is.infinite(fit$se)) | any(is.na(fit$se))  ){
      values_ci = 0
    }else{
      values_ci = 1.96 * fit$se
    }
    
    # set values to NA if interpolated
    # max gap is 10 days, to avoid flagging periods where
    # you only lack some data
    # this is redundant should only do this once (fix)
    int = na.approx(values, maxgap = 10)
    
    # set values smaller than a basic carry forward
    # interpolation to the carry forward value
    cf = na.locf(values, na.rm=FALSE)
    values_smooth[which(is.na(int) & values_smooth < cf)] = cf[which(is.na(int) & values_smooth < cf)]
    
    # put everything in the output matrix
    output$int_flag[which(is.na(int))] = 1
    output[,which(colnames(output) == sprintf("smooth_%s",i))] = values_smooth
    output[,which(colnames(output) == sprintf("smooth_ci_%s",i))] = values_ci
  }
  
  # drop previously smoothed data from
  # a data frame
  dropvar = names(df) %in% column_names
  df = df[!dropvar]
  df = cbind(df,output)
  
  # if the data is not a data frame, write
  # to the same file else return df
  if(!is.data.frame(filename)){
    # writing the final data frame to file, retaining the original header
    write.table(header,filename,quote=FALSE, row.names=FALSE, col.names=FALSE, sep="")
    write.table(df,filename,quote=FALSE, row.names=FALSE, col.names=TRUE, sep=",",append = TRUE)
  }else{
    return(df)
  }
}
