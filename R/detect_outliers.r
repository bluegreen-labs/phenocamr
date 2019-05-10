#' Detect outliers in PhenoCam time series
#' 
#' The function fills in the existing column to hold outlier flags,
#' and either overwrites the original file or outputs a data structure.
#'
#' @param data PhenoCam data structure or filename
#' @param iterations number of itterations in order to detect outliers ()
#' @param sigma number of deviations to exclude outliers at
#' @param grvi reverse the direction of the screening intervals
#' to accomodate for GRVI outliers
#' @param internal return a data structure if given a file on disk
#' (\code{TRUE} / \code{FALSE} = default)
#' to accomodate for GRVI outliers
#' @param plot visualize the process, mostly for debugging
#' (\code{TRUE} / \code{FALSE} = default)
#' @param snow_flag use manual snow flag labels as outliers
#' @param out_dir output directory where to store data 
#' @keywords PhenoCam, outliers, post-processing
#' @export
#' @examples
#'
#' \donttest{
#' # download demo data (do not detect outliers)
#' download_phenocam(site = "harvard$",
#'                   veg_type = "DB",
#'                   roi_id = "1000",
#'                   frequency = "3",
#'                   outlier_detection = FALSE)
#'
#' # detect outliers in the downloaded file
#' detect_outliers(file.path(tempdir(),"harvard_DB_1000_3day.csv"))
#'
#' }

detect_outliers = function(data,
                           iterations=20,
                           sigma = 2,
                           grvi = FALSE,
                           snow_flag = FALSE,
                           plot = FALSE,
                           internal = TRUE,
                           out_dir = tempdir()){

  # specify the double exponential (laplace) distribution
  # standard deviation function, used to screen outliers
  laplace_sd <- function(x,...){
    n <- length(!is.na(x))
    xbar <- mean(x,na.rm=T)
    sqrt(2) * sum(abs(x - xbar),na.rm=T) / n
  }

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
  
  # create year column in df / will be removed in new
  # data files as the year column exists
  month = as.numeric(format(as.Date(df$date),"%m"))
  df$year = as.numeric(format(as.Date(df$date),"%Y"))

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
      df_subset = df[c(dec,yr,jan),]

      # create date and greenness vectors
      dates = strptime(df_subset$date,"%Y-%m-%d")
      gcc = df_subset[,which(colnames(df_subset)==sprintf("gcc_%s",k))]
      gcc_orig = gcc

      # skip year if the gcc vector is (almost) empty
      gcc_length = length(which(gcc != "NA"))
      if (gcc_length <= 3){
        next
      }

      # grab the locations of this years data
      current_year_loc = which(as.numeric(format(dates,"%Y")) == i )

      # calculate the amplitude, and threshold t
      # of the time series
      upper = stats::quantile(gcc,0.9,na.rm=T)
      lower = stats::quantile(gcc,0.1,na.rm=T)
      amp = upper - lower
      t = amp / 4

      # drop NA's, messes with diff()
      gcc_change = c(NA,diff(gcc[!is.na(gcc)]))

      # select days that drop more than 1/4 in amplitude
      selection = which(gcc_change < ( t * -1 ) )

      outliers = rep(0,length(gcc))
      tmp_loc = c() # temporary outlier locations

      # calculate variability (threshold of values to keep)
      # between days
      daily_var = laplace_sd(gcc_change,na.rm=T)

      # if the dialy variability is very small be less
      # restrictive on selecting outliers. Low variability
      # series have little structure, removing outliers very
      # stringently imposes artificial patterns. Basically
      # these series are ~flat and should remain so.

      # the current.sigma value for outlier detection is based
      # upon this value cut-off value or the GRVI flag if dealing
      # with GRVI data. The GRVI flag will be rolled into the formal
      # processing if this data becomes available in the standard
      # processed files
      if (daily_var <= 0.007){
        sigma_current = 2 * sigma
      } else {
        sigma_current = sigma
      }

      # set sigma for grvi
      if (grvi == TRUE){
        sigma_current = 1
      }

      # set iterator for while loop
      j = 1
      while (j < iterations){

        if (j == 1){

          gcc[selection] = NA

        }

        # calculate optimal span / smoothing factor
        span = suppressWarnings(optimal_span(gcc, step = 0.01))

        # remove old projections
        if ( exists('pred') ){
          rm('pred')
        }

        if (is.null(span)) {

          gcc_smooth = gcc

        } else {

          # calculate fit using the optimal span / smoothing factor
          fit = suppressWarnings(stats::loess(gcc ~ as.numeric(dates), span = span))

          # predict values using the fit
          pred = suppressWarnings(stats::predict(fit,as.numeric(dates), se = TRUE))

          # loess data
          gcc_smooth = pred$fit

          # standard error
          #gcc_sd = pred$se * 1.96
          #daily.var = pred$se * 1.96
        }

        # get the difference
        gcc_dif = gcc_orig - gcc_smooth

        # calculate outliers (up or down), change direction of the
        # assymetrical criteria for the GRVI (outliers are upward)
        if (grvi == FALSE){
          loc_up = which(gcc_dif > 2 * sigma_current * daily_var )
          loc_down = which(gcc_dif <= 1 * sigma_current * -daily_var )
          loc = c(loc_up,loc_down)
        } else { # GRVI specific routine
          loc_up = which(gcc_dif > 0.5 * sigma_current * daily_var )
          loc_down = which(gcc_dif <= sigma_current * -daily_var )
          loc = c(loc_up,loc_down)
        }
        # only retain last iteration values
        # in the next iteration
        gcc = gcc_orig # reset to original values
        gcc[loc] = NA # remove current outliers
        outliers[loc] = 1
        outliers[-loc] = 0

        # break conditions (when no change is detected),
        # if not met update the temporary location vector
        if (sum(tmp_loc - loc) == 0 & j != 1){
          break # exit while loop
        }else{

          # visualize iterations, for debugging
          if ( plot == TRUE ){
            graphics::par(mfrow=c(1,1))
            plot(dates,gcc_orig)
            graphics::points(dates[loc],gcc_orig[loc],col='red',pch=19)

            if ( exists('pred') ){
              graphics::lines(dates,pred$fit)
            }
            Sys.sleep(1)
          }

          # overwrite previous locations with
          # the new ones (kept for the next
          # iteration)
          tmp_loc = loc
        }

        # increase the counter, go again...
        j = j + 1
      }

      # put everything back into the dataframe
      if (k == "mean"){
        df$outlierflag_gcc_mean[df$year == i] = outliers[current_year_loc]
      }
      if (k == 90){
        df$outlierflag_gcc_90[df$year == i] = outliers[current_year_loc]
      }
      if (k == 75){
        df$outlierflag_gcc_75[df$year == i] = outliers[current_year_loc]
      }
      if (k == 50){
        df$outlierflag_gcc_50[df$year == i] = outliers[current_year_loc]
      }
      if (k == "grvi"){
        df$outlierflag_gcc_grvi[df$year == i] = outliers[current_year_loc]
      }

    } # loop over years

    # Only consider manual snow flag labels
    # This can be done outside the next yearly loop (vector operation)
    # optional using parameter
    if (snow_flag){  
    
      # accounting for the days when there is snow flag data available
      snowflagged_indices <- (df$snow_flag==1)
      snowflagged_indices[is.na(snowflagged_indices)] <- FALSE # treating NA's as no-snow

      # put everything back into the dataframe
      if (k == "mean"){
        df$outlierflag_gcc_mean[snowflagged_indices] = 1
      }
      if (k == 90){
        df$outlierflag_gcc_90[snowflagged_indices] = 1
      }
      if (k == 75){
        df$outlierflag_gcc_75[snowflagged_indices] = 1
      }
      if (k == 50){
        df$outlierflag_gcc_50[snowflagged_indices] = 1
      }
    }

  } # loop over metrics

  # put data back into the data structure
  data$data = df
  
  # write the data to the original data frame or the
  # original file (overwrites the data!!!)
  if(on_disk | !internal ){
    write_phenocam(data, out_dir = out_dir)
  } else {
    # if provided a data frame
    # return the original data frame, with flagged outliers
    return(data)
  }
}