#' Smooths a standard PhenoCam file or data frame using
#' the BCI optimized smoothing parameter
#' @param df a PhenoCam data file or data frame
#' @param metrics which metrics to process, normally all default ones
#' @param force TRUE / FALSE, force reprocessing?
#' @keywords time series, smoothing, phenocam
#' @export
#' @examples
#'
#' \dontrun{
#' # with defaults, outputting a data frame
#' # with smoothed values, overwriting the original
#'
#' # download demo data (do not smooth)
#' download.phenocam(site="harvard",
#'                   vegetation="DB",
#'                   roi_id=1,
#'                   frequency=3,
#'                   smooth=FALSE)
#'
#' # smooth the downloaded file (and overwrite the original)
#' smooth_ts("harvard_DB_0001_1day.csv")
#'
#' # the function also works on a PhenoCam data frame
#' # but you will lose the extensive header in the process
#' df = read.csv("harvard_DB_0001_1day.csv")
#' df = smooth_ts(df)
#' }

smooth_ts = function(df,
                     metrics = c("gcc_mean",
                                 "gcc_50",
                                 "gcc_75",
                                 "gcc_90",
                                 "rcc_mean",
                                 "rcc_50",
                                 "rcc_75",
                                 "rcc_90"),
                     force = TRUE) {

  # if the data is not a data frame, load
  # the file (assuming it is a phenocam)
  # summary file, otherwise rename the
  # input data to df

  # check if it's a filename or data frame
  df_check = is.data.frame(df)

  if (!df_check) {
    if (file.exists(df)) {

      # assign filename
      filename = df

      # read data file
      header = try(readLines(df, n = 22), silent = TRUE)

      # read in the data frame
      df = utils::read.table(df, header = TRUE, sep = ",")

    } else{
      stop("not a valid PhenoCam data frame or file")
    }
  }

  # detect frequency
  freq = median(diff(na.omit(df$doy)))
  
  # if it's a smoothed file, bail unless you want to
  # force the smoothing again
  if (any(grepl("smooth_*", colnames(df))) & force == FALSE) {
    stop("data is already smoothed")
  }

  # maximum allowed gap before the whole stretch is
  # flagged as too long to be reliably interpolated
  maxgap = 14

  # create convenient date vector
  # (static for all data)
  dates = as.Date(df$date)
  
  # create output matrix
  output = matrix(NA, length(dates), length(metrics) * 2 + 1)
  output = as.data.frame(output)
  column_names = c(sprintf("smooth_%s", metrics),
                   sprintf("smooth_ci_%s", metrics),
                   "int_flag")
  colnames(output) = column_names

  # loop over all metrics that need smoothing
  for (i in metrics) {

    # get the values to use for smoothing
    values = df[, which(colnames(df) == i)]

    # flag all outliers as NA
    # if the metric is gcc based
    if (grepl("gcc", i)) {
      outliers = df[, which(colnames(df) == sprintf("outlierflag_%s", i))]
      values[outliers == 1] = NA
    }

    # create yearly mean values and fill in time series
    # with those, keep track of which values are filled
    # using the int_flag data
    nr_years = length(unique(df$year))

    # find the location of the original NA values
    # to use to fill these gaps later
    na_orig = which(is.na(values))

    # na locations (default locations for 3-day product)
    # this to prevent inflation of the number of true
    # values in the 3-day product
    loc = seq(2,366,3)
    loc = (df$doy %in% loc)
    
    # Calculate the locations of long NA gaps.
    # (find remaining NA values after interpolation,
    # limited to 2 weeks in time)
    long_na = which(is.na(zoo::na.approx(
      values, maxgap = maxgap, na.rm = FALSE
    )))
    
    # also find the short gaps (inverse long gaps)
    # to smooth spikes
    short_na = which(!is.na(zoo::na.approx(
      values, maxgap = maxgap, na.rm = FALSE
    )))
    short_na = which(short_na %in% is.na(values))
    
    # this routine takes care of gap filling large gaps
    # using priors derived from averaging values across
    # years or linearly interpolating. The averaging over
    # years is needed to limit artifacts at the beginning
    # and end of cycles in subsequent phenophase extraction
    if (nr_years >= 2) {
      
      # used to be 3, fill values using those of the remaining year

      # calculate the mean values for locations
      # where there are no values across years
      fill_values = by(values,INDICES = df$doy, mean, na.rm = TRUE)
      doy_fill_values = as.numeric(names(fill_values))
      #doy_na = df$doy[na_orig]
      doy_na = df$doy[long_na]
      
      # calculate the interpolated data based on
      # the whole dataset
      int_data = unlist(lapply(doy_na,
                    function(x,...) {
                      fv = fill_values[which(doy_fill_values == x)]
                      if (length(fv) == 0) {
                        return(NA)
                      }else{
                        return(fv)
                      }
                    }))
      
      # gap fill the original dataset using
      # the interpolated values
      gap_filled_prior = values
      #gap_filled_prior[na_orig] = int_data
      gap_filled_prior[long_na] = int_data
      
      # reset NA short sections to NA and interpolate these linearly
      # only long NA periods merit using priors
      gap_filled_prior[short_na] = NA
      gap_filled_linear = zoo::na.approx(gap_filled_prior, na.rm = FALSE)

      # the above value should be independent of the ones used in the carry
      # forward / backward exercise

      # traps values stuck at the end in NA mode, use carry
      # forward and backward to fill these in! These errors
      # don't pop up when using a fitting model (see above)
      gap_filled_forward = zoo::na.locf(gap_filled_linear,
                                        na.rm = FALSE)
      gap_filled_backward = zoo::na.locf(gap_filled_linear,
                                         na.rm = FALSE,
                                         fromLast = TRUE)

      # drop in values at remaining NA places
      gap_filled_forward[is.na(gap_filled_forward)] = gap_filled_backward[is.na(gap_filled_forward)]
      gap_filled_backward[is.na(gap_filled_backward)] = gap_filled_forward[is.na(gap_filled_backward)]

      # take the mean of the carry forward and backward run
      # this should counter some high or low biases by using the
      # average of last or first value before or after an NA stretch
      gap_filled_linear = ( gap_filled_forward + gap_filled_backward ) / 2
      gap_filled = apply(cbind(gap_filled_prior,gap_filled_linear),1,max,na.rm=TRUE)

    }else{

      # for short series, where averaging over years isn't possible
      # linearly interpolate the data for gap filling
      # it's not ideal (no priors) but the best you have
      gap_filled = zoo::na.approx(values, na.rm = FALSE)

      # traps values stuck at the end in NA mode, use carry
      # forward and backward to fill these in! These errors
      # don't pop up when using a fitting model (see above)
      gap_filled = zoo::na.locf(gap_filled, na.rm = FALSE)
      gap_filled = zoo::na.locf(gap_filled, na.rm = FALSE, fromLast = TRUE)
    }

    # the gap_filled object is used in the subsequent analysis
    # to calculate the ideal fit, down weighing those areas
    # which were interpolated

    # create weight vector for original NA
    # values and snow flag data
    weights = rep(1,length(values))
    weights[na_orig] = 0.001
    #weights[df$snow_flag == 1] = 0.001
    
    # smooth input series for plotting
    # set locations to NA which would otherwise not exist in the
    # 3-day product, as not to inflate the number of measurements
    if (freq == 3){
      
      optim_span = optimal_span(x = as.numeric(dates[loc]),
                                y = gap_filled[loc],
                                plot = FALSE) # plot for debugging
      
      fit = loess(gap_filled[loc] ~ as.numeric(dates[loc]),
                  span = optim_span,
                  weights = weights[loc])

    } else { # 1-day product

      optim_span = optimal_span(x = as.numeric(dates),
                                y = gap_filled,
                                plot = FALSE) # plot for debugging

      fit = loess(gap_filled ~ as.numeric(dates),
                  span = optim_span,
                  weights = weights)

    }

    # make projections based upon the optimal fit
    fit = predict(fit, as.numeric(dates), se = TRUE)

    # grab the smoothed series and the CI (from SE)
    # set to 0 if no SE is provided
    values_smooth = fit$fit

    # calculate the CI (from SE)
    values_ci = 1.96 * fit$se

    # cap CI values to 0.02
    values_ci[values_ci > 0.02] = 0.02

    # trap trailing and starting NA values
    values_smooth = zoo::na.locf(values_smooth, na.rm=FALSE)
    values_smooth = zoo::na.locf(values_smooth, fromLast = TRUE, na.rm=FALSE)

    # set values for long interpolated values to 0
    # these are effectively missing or inaccurate
    # (consider setting those to NA, although this
    # might mess up plotting routines)
    values_ci[long_na] = 0.02 # previously 0 now 0.02
    
    # trap values where no CI was calculated and
    # assign the fixed value
    values_ci[is.nan(fit$se)] = 0.02
    values_ci[is.na(fit$se)] = 0.02
    values_ci[is.infinite(fit$se)] = 0.02
    
    # set values to NA if interpolated
    # max gap is 'maxgap' days, to avoid flagging periods where
    # you only lack some data
    # this is redundant should only do this once (fix)
    int = zoo::na.approx(values, maxgap = maxgap, na.rm = FALSE)

    # put everything in the output matrix
    output$int_flag[which(is.na(int))] = 1
    output[, which(colnames(output) == sprintf("smooth_%s", i))] = round(values_smooth,5)
    output[, which(colnames(output) == sprintf("smooth_ci_%s", i))] = round(values_ci,5)

    cols = rep("red",length(gap_filled))
    cols[long_na] = "green"
  }

  # drop previously smoothed data from
  # a data frame
  dropvar = names(df) %in% column_names
  df = df[!dropvar]
  df = cbind(df, output)

  # if the data is not a data frame, write
  # to the same file else return df
  if (!df_check) {

    # writing the final data frame to file, retaining the original header
    write.table(
      header,
      filename,
      quote = FALSE,
      row.names = FALSE,
      col.names = FALSE,
      sep = ""
    )
    write.table(
      df,
      filename,
      quote = FALSE,
      row.names = FALSE,
      col.names = TRUE,
      sep = ",",
      append = TRUE
    )
  } else{
    return(df)
  }
}
