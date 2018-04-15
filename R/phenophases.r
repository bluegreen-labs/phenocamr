#' Calculates phenophases, both greenup and senescence, for all 'seasons'
#' in a PhenoCam time series. This routine combines a forward and backward
#' run of transition_dates function to calculate the phenophases.
#'
#' @param data a PhenoCam data file (or data frame)
#' @param mat mean annual temperature
#' @param internal return PhenoCam data file or data frame
#' @param out_dir output directory
#' @param ... pass parameters to the transition_dates() function
#' @keywords PhenoCam, transition dates, phenology, time series
#' @export
#' @examples
#'
#' \donttest{
#' # downloads a time series for Bartlett Forest data and calculates
#' # the matching phenophases.
#' # Outputs a nested list of phenophases dates
#' # where location [[1]] holds the greenup dates and location
#' # [[2]] the greendown dates
#' download_phenocam(site = "harvard$",
#'                   veg_type = "DB",
#'                   roi_id = "1",
#'                   frequency = "3")
#'                   
#' df = utils::read.csv(paste0(tempdir(),"/harvard_DB_0001_3day.csv"))
#' my_dates = phenophases(df, output = FALSE)
#' }

phenophases = function(data,
                       mat = NULL,
                       internal = TRUE,
                       out_dir=tempdir(),
                       ...
                       ){

  # if the data is not a data frame, load
  # the file (assuming it is a phenocam)
  # summary file, otherwise rename the
  # input data to df
  if(class(data) != "phenocamr"){
    if(file.exists(data)){
      data = read_phenocam(data)
      on_disk = TRUE
    } else {
      stop("not a valid PhenoCam data frame or file")
    }
  } else {
    on_disk = FALSE
  }
  
  # split out data from read in or provided data
  df = data$data

  # set threshold values
  mat_threshold = 10

  # if no mean annual temperature is provided
  # set to the threshold value, invalidating
  # further screening
  if (is.null(mat)){
    mat = mat_threshold
  }
  
  # calculate rising greenness transtions dates
  # all percentiles
  percentiles = c(90,75,50,"mean")
  
  rising = do.call("rbind", lapply(percentiles, function(i){

    # calculate the transition dates
    tmp = suppressWarnings(transition_dates(data,
                           reverse = FALSE,
                           percentile = i,
                           ...))
    
    # screen for false rising parts
    loc = strptime(as.Date(tmp$transition_10, origin = "1970-01-01"),
                   "%Y-%m-%d")$yday
    l = which(loc < 30 | loc > 250)
    if ( mat < mat_threshold & data$veg_type %in% c("DB","SH","GR","EN") & !length(l)==0L ){
      tmp = tmp[-l,]
    }
    
    # formatting output
    gcc_value = rep(sprintf("gcc_%s",i),dim(tmp)[1])
    tmp = cbind(gcc_value, tmp)
    return(tmp)
  }))

  # calculate falling greenness transition dates
  # all percentiles
  falling = do.call("rbind", lapply(percentiles, function(i){
    
    tmp = suppressWarnings(transition_dates(data,
                           reverse = TRUE,
                           percentile = i,
                           ...))
    
    # screen for false falling curves
    loc = strptime(as.Date(tmp$transition_10, origin = "1970-01-01"),
                   "%Y-%m-%d")$yday
    
    
    l = which(loc > 30 & loc < 240)
    if ( mat < mat_threshold & data$veg_type %in% c("DB","SH","GR","EN") & !length(l)==0L ){
      tmp = tmp[-l,]
    }
    
    # formatting output
    gcc_value = rep(sprintf("gcc_%s",i),dim(tmp)[1])
    tmp = cbind(gcc_value, tmp)
    return(tmp)
  }))

  # calculate the RMSE for all spline fits
  smooth_data = df[,grep("smooth_gcc",colnames(df))]
  original_data = df[,grep("^gcc",colnames(df))]
  
  # remove the gcc_std column
  original_data = original_data[,-2]
  
  # calculate the RMSE
  RMSE = round(sqrt(apply((smooth_data - original_data)^2,2,mean,na.rm=TRUE)),5)

  # output data to file
  if (!internal){

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
    site = rep(data$site,rising_length + falling_length)
    veg_type = rep(data$veg_type,rising_length + falling_length)
    roi_id = rep(sprintf("%s",data$roi_id),rising_length + falling_length)

    # convert UNIX time to normal dates
    phenology[, 2:10] = apply(phenology[, 2:10], 2, function(x)
      as.character(as.Date(x, origin = "1970-01-01")))

    # column bind in new labels
    phenology = cbind(site,veg_type,roi_id,direction,phenology)

    # drop NA lines if any
    phenology = stats::na.omit(phenology)

    # create header with output information
    # can be done with one sprintf statement
    # and \n line endings, fix
    phenology_header = paste(
      "#\n",
      sprintf("# Transition date estimate for %s\n",data$site),
      "#\n",
      sprintf("# Site: %s\n",data$site),
      sprintf("# Veg Type: %s\n",data$veg_type),
      sprintf("# ROI ID Number: %s\n",data$roi_id),
      sprintf("# Aggregation period: %s\n", data$frequency),
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
      '%s/%s_%s_%s_%s_transition_dates.csv',
      out_dir,
      data$site,
      data$veg_type,
      data$roi_id,
      data$frequency
    )

    # write all data to file, first write the header then append the data
    utils::write.table(
      phenology_header,
      filename,
      row.names = FALSE,
      col.names = FALSE,
      quote = FALSE
    )

    suppressWarnings(utils::write.table(
      phenology,
      filename,
      row.names = FALSE,
      col.names = TRUE,
      quote = FALSE,
      sep = ',',
      append = TRUE
    ))

  } else {
    # return dates as a list if no output file is required
    return(list("rising" = rising,
                "falling" = falling))
  }
}
