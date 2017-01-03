#' Smooths a standard PhenoCam file or data frame using
#' the BCI optimized smoothing parameter
#' @param df: a PhenoCam data file or data frame
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

smooth.ts = function(df,
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
  maxgap = 10

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
    # if the metric is gcc based
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

    # this routine takes care of gap filling large gaps
    # using priors derived from averaging values across
    # years or linearly interpolating. The averaging over
    # years is needed to limit artifacts at the beginning
    # and end of cycles in subsequent phenophase extraction
    if (nr_years >= 3) {

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
      gap_filled = values
      gap_filled[na_orig] = unlist(int_data)

    }else{

      # for short series, where averaging over years isn't possible
      # linearly interpolate the data for gap filling
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

    # create weight vector
    weights = rep(1,length(values))
    weights[na_orig] = 0.001

    # smooth input series for plotting
    optim.span = optimal.span(gap_filled,weights = weights)
    fit = loess(gap_filled ~ as.numeric(dates), span = optim.span, weights = weights)
    fit = predict(fit, as.numeric(dates), se = TRUE)

    # grab the smoothed series and the CI
    # set to 0 if no SE is provided
    values_smooth = fit$fit

    # calculate the SE
    values_ci = 1.96 * fit$se

    # set values for long interpolated values to 0
    # these are effectively missing or inaccurate
    # (consider setting those to NA, although this
    # might mess up plotting routines)
    values_ci[long_na] = 0

    # skip further processing if the sum of the
    # differences between the true data and the
    # smooth data is 0, an indication of overfitting
    if (sum(values_smooth - values, na.rm = TRUE) == 0 |
        any(is.infinite(fit$se)) | any(is.na(fit$se)) ) {

      # if the latter is true, return the original
      # values as smoothed values
      values_smooth = gap_filled
      values_ci = 0
    }

    # set values to NA if interpolated
    # max gap is 10 days, to avoid flagging periods where
    # you only lack some data
    # this is redundant should only do this once (fix)
    int = zoo::na.approx(values, maxgap = maxgap)

    # put everything in the output matrix
    output$int_flag[which(is.na(int))] = 1
    output[, which(colnames(output) == sprintf("smooth_%s", i))] = round(values_smooth,5)
    output[, which(colnames(output) == sprintf("smooth_ci_%s", i))] = round(values_ci,5)
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
