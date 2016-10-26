#' Calculates phenophases, both greenup and senescence, for all 'seasons'
#' in a PhenoCam time series. This routine combines a forward and backward
#' run of transition.dates() to calculate the phenophases.
#' 
#' @param df: a PhenoCam data file (or data frame)
#' @param sitename: a PhenoCam data file or data frame
#' @param veg_type: a PhenoCam data file or data frame
#' @param roi_id: a PhenoCam data file or data frame
#' @param freq: a PhenoCam data file or data frame
#' @param output: a PhenoCam data file or data frame
#' @keywords PhenoCam, transition dates, phenology, time series
#' @export
#' @examples
#' 
#' # downloads a time series for Bartlett Forest data and calculates
#' # the matching phenophases.
#' # Outputs a nested list of phenophases dates
#' # where location [[1]] holds the greenup dates and location
#' # [[2]] the greendown dates
#' 
#' # df = download.phenocam(site="harvard",
#' #                        type="DB",
#' #                        roi="1",
#' #                        frequency=3)
#' # df = read.csv("harvard_DB_0001_1day.csv")
#' # my_dates = phenophases(df,output=FALSE)
#' 
#' # dates need to be converted to standard notation using
#' # as.Date(my_dates)

phenophases = function(df,
                        sitename=NULL,
                        veg_type=NULL,
                        roi_id=NULL,
                        freq=NULL,
                        lat=NULL,
                        MAT=NULL,
                        output = FALSE,
                        out_dir=getwd()){
  
  # check if the output directory exists
  #if (!dir.exists(out_dir)) {
  #   stop("provide a valid output directory")
  #}
  
  # load data and check input parameters
  if (!is.data.frame(df)) {
    if (file.exists(df)) {
      
      # read header of the file
      header = readLines(file(df),n=22)
      
      # unravel list
      header = strsplit(header,split=":")
      l = lapply(header,length)
      header = unlist(lapply(header[which(l>1)],"[[",2))
      
      # remove leading spaces
      header = gsub(" ","",header)
      
      # extract metadata from the header field
      sitename = header[1]
      veg_type = header[2]
      roi_id = as.numeric(header[3])
      freq = header[9]
      lat = as.numeric(header[4])
      #MAT = as.numeric(header[x])
      
      # read the original data
      df = utils::read.table(df, header = T, sep = ',')
    } else {
      stop("not a valid PhenoCam data frame or file")
    }
    
  } else {
    
    # check if I have all parameters if fed in a data frame
    # [can't be derived from data frame itself]
    if ( any(is.null(c(sitename,veg_type,roi_id,freq,MAT,lat))) ){
      stop("Not all required parameters provided")
    }
  }
  
  # calculate the daylength for additional screening
  # calculate for a single year, not the time series
  dl = daylength(1:366,lat)[[1]]
  dl_trend = ifelse(c(diff(dl),NA)<=0,1,0)
  
  # calculate rising greenness transtions dates
  # all percentiles
  for(i in c(90,75,50,"mean")){
   if(i == 90){
     
     rising = transition.dates(df,reverse=FALSE,percentile=i)
     
     # screen for false rising parts
     locr = strptime(as.Date(rising$transition_10),"%Y-%m-%d")$yday
     l = which(dl_trend[locr] == 1 )
     if(!length(l)==0L){
       rising = rising[-l,]
     }
     
     gcc_value = rep(sprintf("gcc_%s",i),dim(rising)[1])
     rising = cbind(gcc_value,rising)
     
   } else {
     tmp = transition.dates(df,reverse=FALSE,percentile=i)

     # screen for false rising parts
     loc = strptime(as.Date(tmp$transition_10),"%Y-%m-%d")$yday
     l = which(dl_trend[loc] == 1 )
     if(!length(l)==0L){
       tmp = tmp[-l,]
     }
     
     gcc_value = rep(sprintf("gcc_%s",i),dim(tmp)[1])     
     
     tmp = cbind(gcc_value,tmp)
     rising = rbind(rising,tmp)
   }
  }
  
  # calculate falling greenness transition dates
  # all percentiles
  for(i in c(90,75,50,"mean")){
    if(i == 90){
      falling = transition.dates(df,reverse=TRUE,percentile=i)
      
      # screen for false falling curves
      loc = strptime(as.Date(falling$transition_50),"%Y-%m-%d")$yday
      l = which(dl_trend[loc] == 0 )
      if(!length(l)==0L){
        falling = falling[-l,]
      }
      
      gcc_value = rep(sprintf("gcc_%s",i),dim(falling)[1])
      falling = cbind(gcc_value,falling)
    } else {
      tmp = transition.dates(df,reverse=TRUE,percentile=i)
      
      # screen for false falling curves
      loc = strptime(as.Date(tmp$transition_50),"%Y-%m-%d")$yday
      l = which(dl_trend[loc] == 0 )
      if(!length(l)==0L){
        tmp = tmp[-l,]
      }
      
      gcc_value = rep(sprintf("gcc_%s",i),dim(tmp)[1])
      tmp = cbind(gcc_value,tmp)
      falling = rbind(falling,tmp)
    }
  }
  
  # calculate the RMSE for all spline fits
  # (matrix way)
  smooth_data = df[,grep("smooth_gcc",colnames(df))]
  original_data = df[,grep("^gcc",colnames(df))]
  original_data = original_data[,-2]
  s = (smooth_data - original_data)^2
  ss = apply(s,2,sum,na.rm=TRUE)
  RMSE = round(sqrt(ss/dim(smooth_data)[2]),5)

  #plot(as.Date(df$date),df$gcc_90, pch=20,xlab="Date",ylab="Gcc")
  #legend("topright",legend="daylength trend screening",bty="n")
  #abline(v=as.Date(rising$transition_50),col='green')
  #abline(v=as.Date(falling$transition_50),col='red')
  
  # output data to file
  if (output){
    
  # bind spring and fall phenology data in a coherent format
  phenology = rbind(rising,falling)
  
  # get the nr rows for each run
  rising_length = dim(rising)[1]
  falling_length = dim(falling)[1] 
  
  # create a string for the direction of the analysis
  # rising or falling curves
  direction = c(rep("rising", rising_length), 
                rep("falling", falling_length))
  
  # same for sitename, veg_type and roi_id
  sitename = rep(sitename,rising_length + falling_length)
  veg_type = rep(veg_type,rising_length + falling_length)
  roi_id = rep(sprintf("%04d",roi_id),rising_length + falling_length)
  
  # convert UNIX time to normal dates
  phenology[, 2:10] = apply(phenology[, 2:10], 2, function(x)
    as.character(as.Date(x)))
  
  # column bind in new labels
  phenology = cbind(sitename,veg_type,roi_id,direction,phenology)
  
  # drop NA lines if any
  phenology = na.omit(phenology)
  
  # create header with output information
  # can be done with one sprintf statement
  # and \n line endings, fix
  
  phenology_header = paste(
        "#\n",
        sprintf("# Transition date estimate for %s\n",sitename[1]),
        "#\n",
        sprintf("# Site: %s\n",sitename[1]),
        sprintf("# Veg Type: %s\n",veg_type[1]),
        sprintf("# ROI ID Number: %s\n",roi_id[1]),
        sprintf("# Aggregation period: %s\n", freq),
        sprintf("# Year min: %s\n",
                min(strptime(as.matrix(phenology[, 6:14]),"%Y-%m-%d")$year+1900),
                na.rm=TRUE),
        sprintf("# Year max: %s\n",
                max(strptime(as.matrix(phenology[, 6:14]),"%Y-%m-%d")$year+1900),
                na.rm=TRUE),
        sprintf("# Final Processing Date: %s\n", Sys.Date()),
        sprintf("# Final Processing Time: %s\n", format(Sys.time(), "%H:%M:%S")),
        sprintf("# Spline RMSE gcc_mean: %s\n", RMSE[1]),
        sprintf("# Spline RMSE gcc_50: %s\n", RMSE[2]),
        sprintf("# Spline RMSE gcc_75: %s\n", RMSE[3]),
        sprintf("# Spline RMSE gcc_90: %s\n", RMSE[4]),
        "#",
        sep='')
  
  # set filename
  filename = sprintf(
    '%s/transition_dates_%s_%s_%s_%sday.csv',
    out_dir,
    sitename[1],
    veg_type[1],
    roi_id[1],
    freq
  )
  
  # write all data to file, first write the header then append the data
  utils::write.table(
    phenology_header,
    filename,
    row.names = FALSE,
    col.names = FALSE,
    quote = FALSE
  )
  
  utils::write.table(
    phenology,
    filename,
    row.names = FALSE,
    col.names = TRUE,
    quote = FALSE,
    sep = ',',
    append = TRUE
  )
  
  } else {
    # return dates as a list if no output file is required
    return(list(rising,falling))
  }
}