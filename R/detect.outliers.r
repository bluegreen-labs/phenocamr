#' Detect outliers in PhenoCam time series (data frame or file)
#' The function fills in the existing column to hold outlier flags,
#' and either overwrites the original file or outputs a new data frame.
#' 
#' @param data: PhenoCam data frame or filename
#' @param iterations: number of itterations over which to cycle to detect outliers ()
#' @param grvi: reverse the direction of the screening intervals
#' to accomodate for GRVI outliers
#' @param vis: visualize the process, mostly for debugging (TRUE / FALSE)
#' @param snowflag: integrate snow flags?
#' @keywords PhenoCam, outliers, smoothing, pre-processing
#' @export
#' @examples
#' 
#' # download demo data (do not detect outliers)
#' # download.phenocam(site="harvard",
#' #                   vegetation="DB",
#' #                   roi_id=1,
#' #                   frequency=3,
#' #                 outlier_detection=FALSE)
#' 
#' # detect outliers in the downloaded file
#' # detect.outliers("harvard_DB_0001_1day.csv")
#' 
#' # This function is rarely used stand-alone as it is called by the
#' # download.phenocam() function and does not serve any other purpose
#' # as it needs the outlier_flag_xxx field to work (and does not verify this).
#' # Use stand-alone at your own risk.

detect.outliers = function(data,
                           iterations=10,
                           grvi=FALSE,
                           vis=FALSE,
                           snowflag=FALSE ){
  
  # specify the double exponential (laplace) distribution 
  # standard deviation function, used to screen outliers
  laplace.sd <- function(x,...){  
    n <- length(!is.na(x))
    xbar <- mean(x,na.rm=T)  
    sqrt(2) * sum(abs(x - xbar),na.rm=T) / n
  }
  
  # if the data is not a data frame, load
  # the file (assuming it is a phenocam)
  # summary file, otherwise rename the
  # input data to df
  if(!is.data.frame(data)){
    if(file.exists(data)){
      # pluck real header from the phenocam file
      header = readLines(data,n=22)
      
      # read the original data
      df = utils::read.table(data,header=T,sep=',')
    }else{
      stop("not a valid PhenoCam data frame or file")
    }
  } else {
    df = data
  }
  
  # create year column in df / will be removed in new
  # data files as the year column exists
  month = as.numeric(format(as.Date(df$date),"%m"))
  df$year = as.numeric(format(as.Date(df$date),"%Y")) # just to be sure overwrite year column
  
  # which time series to evaluate
  if (grvi == TRUE){
    ts = c("grvi")
  } else {
    ts = c("mean",90,75,50)
  }
  
  # loop over gcc 90 / 75 / 50 time series
  for (k in ts){

    # loop over all years to calculate outliers in each year
    for (i in unique(df$year)){

      # subset the data, with padding of a month to accomodate edge effects
      dec = which(month == 12 & df$year == (i - 1) )
      jan = which(month == 1 & df$year == (i + 1) )
      yr = which(df$year == i)
      df.subset = df[c(dec,yr,jan),]
      
      # create date and greenness vectors
      dates = strptime(df.subset$date,"%Y-%m-%d")
      gcc = df.subset[,which(colnames(df.subset)==sprintf("gcc_%s",k))]
      gcc.orig = gcc
      
      # skip year if the gcc vector is (almost) empty
      gcc.length = length(which(gcc != "NA"))
      if (gcc.length <= 3){
        next
      }
      
      # grab the locations of this years data
      current.year.loc = which(as.numeric(format(dates,"%Y")) == i )
      
      # calculate the amplitude, and threshold t
      # of the time series
      upper = quantile(gcc,0.9,na.rm=T)
      lower = quantile(gcc,0.1,na.rm=T)
      amp = upper - lower 
      t = amp/4
      gcc.change = c(NA,diff(gcc[!is.na(gcc)])) # drop NA's, messes with diff()
      
      # select days that drop more than 1/4 in amplitude
      selection = which(gcc.change < (t*-1)) 
      
      outliers = rep(0,length(gcc))
      tmp.loc = c() # temporary outlier locations
      
      # calculate daily variability (threshold of values to keep)
      daily.var = laplace.sd(gcc.change,na.rm=T)
      
      # if it's larger than that of the maximum
      # of a low noise site, set the scaling factor
      # to be restrictive, if not be more generous
      if (daily.var > 0.007){
        sigma = 2
      } else {
        sigma = 4
      }
      
      # set sigma for grvi
      if (grvi == TRUE){
        sigma = 1
      }
      
      # set iterator for while loop
      j = 1
      while (j < iterations){
        if (j == 1){
         gcc[selection] = NA
        }
        
        # calculate optimal span / smoothing factor
        span = optimal.span(gcc)
        
        # calculate fit using the optimal span / smoothing factor
        fit = suppressWarnings(stats::loess(gcc ~ as.numeric(dates), span = span))
        
        # predict values using the fit
        pred = predict(fit,as.numeric(dates))
        
        # loess data
        gcc_smooth = pred
        
        # get the difference
        gcc.dif = gcc.orig - gcc_smooth

        # calculate outliers (up or down), change direction of the
        # assymetrical criteria for the GRVI (outliers are upward)
        if (grvi == FALSE){
          loc_up = which(gcc.dif > 2 * sigma  * daily.var )
          loc_down = which(gcc.dif <=  sigma * -daily.var )
          loc = c(loc_up,loc_down)
        } else {
          loc_up = which(gcc.dif > sigma  * daily.var )
          loc_down = which(gcc.dif <= sigma * -daily.var )
          loc = c(loc_up,loc_down)
        }
        # only retain last iteration values
        # in the next iteration
        gcc = gcc.orig # reset to original values
        gcc[loc]=NA # remove current outliers
        outliers[loc] = 1
        outliers[-loc] = 0
        
        # break conditions (when no change is detected),
        # if not met update the temporary location vector
        if (sum(tmp.loc - loc) == 0 & j != 1){
          break # exit while loop
        }else{
          
          # visualize iterations, for debugging
          if (vis==TRUE){
            par(mfrow=c(1,1))
            plot(dates,gcc.orig)
            points(dates[loc],gcc.orig[loc],col='red',pch=19)
            lines(dates,pred)
            Sys.sleep(1)
          }
          
          # overwrite previous locations with
          # the new ones (kept for the next
          # iteration)
          tmp.loc = loc
        }
        
        # increase the counter, go again...
        j = j + 1
      }
      
      # put everything back into the dataframe
      if (k == "mean"){
        df$outlierflag_gcc_mean[df$year==i] = outliers[current.year.loc]
      }
      if (k == 90){
        df$outlierflag_gcc_90[df$year==i] = outliers[current.year.loc]
      }
      if (k == 75){
        df$outlierflag_gcc_75[df$year==i] = outliers[current.year.loc]
      }
      if (k == 50){
        df$outlierflag_gcc_50[df$year==i] = outliers[current.year.loc]
      }
      if (k == "grvi"){
        df$outlierflag_gcc_grvi[df$year==i] = outliers[current.year.loc]
      }
      
    } # loop over years
    
    # Bcc > Gcc (a sign of snow / weather contamination).
    # This can be done outside the next yearly loop (vector operation)
    # optional using parameter
    if (snowflag == TRUE){
      
      # gcc / bcc / rcc time series
      midday_gcc = df$midday_g / (df$midday_r + df$midday_g + df$midday_b)
      midday_bcc = df$midday_b / (df$midday_r + df$midday_g + df$midday_b)
      
      # put everything back into the dataframe
      if (k == "mean"){
        df$outlierflag_gcc_mean[midday_bcc > midday_gcc] = 1
      }
      if (k == 90){
        df$outlierflag_gcc_90[midday_bcc > midday_gcc] = 1
      }
      if (k == 75){
        df$outlierflag_gcc_75[midday_bcc > midday_gcc] = 1
      }
      if (k == 50){
        df$outlierflag_gcc_50[midday_bcc > midday_gcc] = 1
      }
    }
    
  } # loop over metrics
  
  # write the data to the original data frame or the
  # original file (overwrites the data!!!)
  if(!is.data.frame(data)){
    if(file.exists(data)){
      # write everything to file using append
      # write the original header first
      utils::write.table(header,data,quote=FALSE,row.names=FALSE,col.names=FALSE)
      utils::write.table(df,data,row.names=FALSE,col.names=TRUE,quote=FALSE,sep=",",append = TRUE)
    }
  } else {
    # if provided a data frame 
    # return the original data frame, with flagged outliers
    return(df)
  }
}