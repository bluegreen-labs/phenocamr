#' Calculates phenophases, both greenup and senescence, for all 'seasons'
#' in a PhenoCam time series. This routine combines a forward and backward
#' run of transition_dates() to calculate the phenophases.
#'
#' @param df a PhenoCam data file (or data frame)
#' @param sitename a PhenoCam data file or data frame
#' @param veg_type a PhenoCam data file or data frame
#' @param roi_id a PhenoCam data file or data frame
#' @param frequency a PhenoCam data file or data frame
#' @param mat mean annual temperature
#' @param output a PhenoCam data file or data frame
#' @param out_dir output directory
#' @param ... pass parameters to the transition_dates() function
#' @keywords PhenoCam, transition dates, phenology, time series
#' @export
#' @examples
#'
#' \dontrun{
#' # downloads a time series for Bartlett Forest data and calculates
#' # the matching phenophases.
#' # Outputs a nested list of phenophases dates
#' # where location [[1]] holds the greenup dates and location
#' # [[2]] the greendown dates
#'
#' df = download_phenocam(site="harvard",
#'                        type="DB",
#'                        roi="1",
#'                        frequency=3)
#' df = read.csv("harvard_DB_0001_1day.csv")
#' my_dates = phenophases(df,output = FALSE)
#'
#' # dates need to be converted to standard notation using
#' # as.Date(my_dates)
#' }

phenophases = function(df,
                       sitename=NULL,
                       veg_type=NULL,
                       roi_id=NULL,
                       frequency=NULL,
                       mat = NULL,
                       output = FALSE,
                       out_dir=getwd(),
                       ...
                       ){

  # check if the output directory exists
  if (!dir.exists(out_dir)) {
    stop("provide a valid output directory")
  }

  # load data and check input parameters
  if (!is.data.frame(df)) {
    if (file.exists(df)) {

      # read header of the file
      header = readLines(df,n = 22)

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
      frequency = header[9]

      # read the original data
      df = utils::read.table(df, header = T, sep = ',')
    } else {
      stop("not a valid PhenoCam data frame or file")
    }

  } else {

    # check if I have all parameters if fed in a data frame
    # [can't be derived from data frame itself]
    if ( any(is.null(c(sitename,veg_type,roi_id,frequency))) ){
      stop("Not all required parameters provided")
    }
  }

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
  for ( i in c(90,75,50,"mean") ){

    # calculate the transition dates
    tmp = transition_dates(df,
                           reverse = FALSE,
                           percentile = i,
                           frequency = frequency,
                           ...)
    
    # screen for false rising parts
    loc = strptime(as.Date(tmp$transition_10, origin = "1970-01-01"),"%Y-%m-%d")$yday
    l = which(loc < 30 | loc > 250)
    if ( mat < mat_threshold & veg_type %in% c("DB","SH","GR","EN") & !length(l)==0L ){
      tmp = tmp[-l,]
    }
    
    # formatting output
    gcc_value = rep(sprintf("gcc_%s",i),dim(tmp)[1])
    tmp = cbind(gcc_value, tmp)
    if (i == 90){
      rising = tmp
    } else {
      rising = rbind(rising,tmp)
    }
  }

  # calculate falling greenness transition dates
  # all percentiles
  for ( i in c(90,75,50,"mean") ){

    tmp = transition_dates(df,
                           reverse = TRUE,
                           percentile = i,
                           frequency = frequency)
    
    # screen for false falling curves
    loc = strptime(as.Date(tmp$transition_10, origin = "1970-01-01"),
                   "%Y-%m-%d")$yday
    l = which(loc > 30 & loc < 240)
    if ( mat < mat_threshold & veg_type %in% c("DB","SH","GR","EN") & !length(l)==0L ){
      tmp = tmp[-l,]
    }
    
    # formatting output
    gcc_value = rep(sprintf("gcc_%s",i),dim(tmp)[1])
    tmp = cbind(gcc_value, tmp)
    if ( i == 90 ){
      falling = tmp
    } else {
      falling = rbind(falling,tmp)
    }
  }

  # calculate the RMSE for all spline fits
  smooth_data = df[,grep("smooth_gcc",colnames(df))]
  original_data = df[,grep("^gcc",colnames(df))]
  
  # remove the gcc_std column
  original_data = original_data[,-2]
  
  # calculate the RMSE
  RMSE = round(sqrt(apply((smooth_data - original_data)^2,2,mean,na.rm=TRUE)),5)

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
      as.character(as.Date(x, origin = "1970-01-01")))

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
      sprintf("# Aggregation period: %s\n", frequency),
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
      '%s/%s_%s_%s_%sday_transition_dates.csv',
      out_dir,
      sitename[1],
      veg_type[1],
      roi_id[1],
      frequency
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
