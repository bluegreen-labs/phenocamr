#' Smooths a standard PhenoCam file or data frame using
#' the BCI optimized smoothing parameter
#' @param df: a PhenoCam data file or data frame
#' @param metrics: which metrics to process, normally all default ones
#' @param force: TRUE / FALSE, force reprocessing?
#' @keywords time series, smoothing, phenocam
#' @export
#' @examples
#'
#' # with defaults, outputting a data frame
#' # with smoothed values, overwriting the original
#'
#' # download demo data (do not smooth)
#' # download.phenocam(site="harvard",
#' #                   vegetation="DB",
#' #                  roi_id=1,
#' #                   frequency=3,
#' #                  smooth=FALSE)
#'
#' # smooth the downloaded file (and overwrite the original)
#' # smooth.ts("harvard_DB_0001_1day.csv")
#'
#' # the function also works on a PhenoCam data frame
#' # but you will lose the extensive header in the process
#' # df = read.csv("harvard_DB_0001_1day.csv")
#' # df = smooth.ts(df)

smooth.ts.corr = function(df,
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

      # grab processing frequency, needed for proper
      # processing use either the file name or the header
      freq = ifelse(grepl("3day",filename) | grepl("3-day",header[2]),
                    3,
                    1)

      # read in the data frame
      df = utils::read.table(df, header = TRUE, sep = ",")

    } else{
      stop("not a valid PhenoCam data frame or file")
    }
  }

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

    # flag all snow_flagged data as NA
    # if the metric is gcc / grvi based
    if (grepl("gcc", i) || i == "grvi") {
      outliers = df[, which(colnames(df) == "snow_flag")]
      values[outliers == 1] = NA
    }

    # create yearly mean values and fill in time series
    # with those, keep track of which values are filled
    # using the int_flag data
    nr_years = length(unique(df$year))

    # find the location of the original NA values
    # to use to fill these gaps later
    na_orig = which(is.na(values))

    # na locations (default locations for 3day product)
    # this to prevent inflation of the number of true
    # values in the 3-day product
    loc = seq(2,366,3)
    loc = (df$doy %in% loc)

    # this routine takes care of gap filling large gaps
    # using priors derived from averaging values across
    # years or linearly interpolating. The averaging over
    # years is needed to limit artifacts at the beginning
    # and end of cycles in subsequent phenophase extraction
    if (nr_years >= 2) {
      # used to be 3, fill values using those of the remaining year

      # calculate the mean values for locations
      # where there are no values across years
      fill_values = by(values,INDICES = df$doy, mean,na.rm = TRUE)
      doy_fill_values = as.numeric(names(fill_values))
      doy_na = df$doy[na_orig]

      # calculate the interpolated data based on
      # the whole dataset
      int_data = lapply(doy_na,
                    function(x,...) {
                      fv = fill_values[which(doy_fill_values == x)]
                      if (length(fv) == 0) {
                        return(NA)
                      }else{
                        return(fv)
                      }
                    })

      # gap fill the original dataset using
      # the interpolated values
      gap_filled_prior = values
      gap_filled_prior[na_orig] = unlist(int_data)
      #gap_filled_linear = gap_filled_prior
      gap_filled_linear = zoo::na.approx(gap_filled_prior, na.rm = FALSE)

      # the above value should be independent of the ones used in the carry
      # forward / backward exercise

      # traps values stuck at the end in NA mode, use carry
      # forward and backward to fill these in! These errors
      # don't pop up when using a fitting model (see above)
      gap_filled_forward = zoo::na.locf(gap_filled_linear, na.rm = FALSE)
      gap_filled_backward = zoo::na.locf(gap_filled_linear, na.rm = FALSE, fromLast = TRUE)

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

    # Calculate the locations of long NA gaps.
    # (find remaining NA values after interpolation,
    # limited to 2 weeks in time)
    long_na = which(is.na(zoo::na.approx(
      values, maxgap = maxgap, na.rm = FALSE
    )))

    # weigh monotenous rising / falling points more
    # to constrain the curve to the most dynamic
    # parts first
    mw = 5

    # rising
    rising = diff(gap_filled ) > 0
    rising[is.na(rising)] = 0
    rising[rising == FALSE] = 0
    rising[rising == TRUE] = 1
    rising = zoo::rollapply(rising,mw,function(x){
      if(sum(x) == mw){
        1
      }else {
        0
      }}, fill = 0)

    # falling
    falling = diff(gap_filled ) < 0
    falling[is.na(falling)] = 0
    falling[falling == FALSE] = 0
    falling[falling == TRUE] = 1
    falling = zoo::rollapply(falling,mw,function(x){
      if(sum(x) == mw){
        1
      }else {
        0
      }}, fill = 0)

    # combine rising and falling dynamics
    # these value locations can be used to
    # weigh continuously rising/falling sections
    # more in order to constrain the smoothing
    # spline (not used by default)
    shape_constraints = rising + falling

    # create weight vector
    weights = rep(1,length(values))
    weights[na_orig] = 0.0001 # PARAMETER

    # smooth input series for plotting
    # set locations to NA which would otherwise not exist in the
    # 3-day product, as not to inflate the number of measurements
    if (freq == 3){

      optim.span = optimal.span(gap_filled[loc], weights = weights[loc])
      fit = loess(gap_filled[loc] ~ as.numeric(dates[loc]), span = optim.span, weights = weights[loc])

    } else {

      optim.span = optimal.span(gap_filled, weights = weights)
      fit = loess(gap_filled ~ as.numeric(dates), span = optim.span, weights = weights)

    }

    # make projections based upon the optimal fit
    fit = predict(fit, as.numeric(dates), se = TRUE)

    # grab the smoothed series and the CI
    # set to 0 if no SE is provided
    values_smooth = fit$fit

    # calculate the SE
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

    # skip further processing if the sum of the
    # differences between the true data and the
    # smooth data is 0, an indication of overfitting
    if (sum(values_smooth - values, na.rm = TRUE) == 0 |
        any(is.infinite(fit$se)) ) {

      #| any(is.na(fit$se))
      # if the latter is true, return the original
      # values as smoothed values
      values_smooth = gap_filled
      values_ci = 0.02
    }

    # set values to NA if interpolated
    # max gap is 10 days, to avoid flagging periods where
    # you only lack some data
    # this is redundant should only do this once (fix)
    int = zoo::na.approx(values, maxgap = maxgap, na.rm = FALSE)

    # put everything in the output matrix
    output$int_flag[which(is.na(int))] = 1
    output[, which(colnames(output) == sprintf("smooth_%s", i))] = round(values_smooth,5)
    output[, which(colnames(output) == sprintf("smooth_ci_%s", i))] = round(values_ci,5)

    #plot(values)
    #lines(values_smooth,col="blue")

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
