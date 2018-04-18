#' Calculates transition dates for a PhenoCam time series
#' 
#' Segments of a PhenoCam time series and calculates threshold based transition
#' dates for all segments. This function is rarely called stand alone and
#' `phenophases()` should be preferred when evaluating PhenoCam time series.
#'
#' @param data a PhenoCam data file or data structure
#' @param lower_thresh the minimum threshold used (default = 0.1)
#' @param middle_thresh the middle threshold used (default = 0.25)
#' @param upper_thresh the maximum threshold used (default = 0.5)
#' @param percentile time series percentiles to process (mean, 50, 75, 90)
#' @param penalty how sensitive is the algorithm, lower is more sensitve (< 0 )
#' @param seg_length minimum length of a segment to be evaluated
#' @param reverse flip the direction of the processing
#' @param plot plot for debugging purposes
#' @return Transition date estimates in UNIX time, including uncertainties
#' and the threshold values estimated for each section of a time series.
#' @keywords PhenoCam, transition dates, phenology, time series
#' @export
#' @examples
#' 
#' \donttest{
#' # download demo data
#' download_phenocam(site = "harvard$",
#'                   veg_type = "DB",
#'                   roi_id = "1",
#'                   frequency = "3")
#'
#' # read the data and calculate transition dates
#' df <- read_phenocam(paste0(tempdir(),"/harvard_DB_0001_3day.csv"))
#' my_dates <- transition_dates(df,
#'                             lower_thresh = 0.1,
#'                             middle_thresh = 0.25,
#'                             upper_thresh = 0.5,
#'                             percentile = 90,
#'                             reverse = FALSE,
#'                             plot = FALSE)
#' }

transition_dates = function(data,
                            lower_thresh = 0.1,
                            middle_thresh = 0.25,
                            upper_thresh = 0.5,
                            percentile = 90,
                            penalty = 0.5,
                            seg_length = 14,
                            reverse = FALSE,
                            plot = FALSE) {

  # error function, outputs empty data file
  # in case of failure
  err_function = function() {

    err_mat = matrix(NA, 1, 14)

    # assign column names if not empty
    colnames(err_mat) =
        c(
          sprintf("transition_%s",lower_thresh*100),
          sprintf("transition_%s",middle_thresh*100),
          sprintf("transition_%s",upper_thresh*100),
          sprintf("transition_%s_lower_ci",lower_thresh*100),
          sprintf("transition_%s_lower_ci",middle_thresh*100),
          sprintf("transition_%s_lower_ci",upper_thresh*100),
          sprintf("transition_%s_upper_ci",lower_thresh*100),
          sprintf("transition_%s_upper_ci",middle_thresh*100),
          sprintf("transition_%s_upper_ci",upper_thresh*100),
          sprintf("threshold_%s",lower_thresh*100),
          sprintf("threshold_%s",middle_thresh*100),
          sprintf("threshold_%s",upper_thresh*100),
          'min_gcc',
          'max_gcc'
        )

    return(as.data.frame(err_mat))
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
  
  # grab processing frequency, needed for proper
  # processing use either the file name or the header
  if (data$frequency == "roistats") {
    stop("not a 1 or 3-day time series file")
  }

  # check if smooth data is present, if not stop
  if (!any(grepl("smooth", colnames(df)))) {
    stop("No smoothed data in data frame, smooth data first using smooth.ts()")
  }
  
  # find days which are valid 3-day product locations
  # this creates consistency between products calculated
  # from data frames or on file
  if (data$frequency == "3day") {
    loc_freq = which(df$doy %in% seq(2,366,3))
  } else {
    loc_freq = 1:nrow(df)
  }
  
  # select original smooth data, needed to grab the
  # threshold values on the original scale for export
  smooth_orig = df[, which(colnames(df) == sprintf("smooth_gcc_%s", percentile))]

  # if all smooth values are NA, bail
  # with default output err_function()
  if ( all(is.na(smooth_orig)) ) {
    return(err_function())
  }

  # put raw data values into a variable
  raw_data = df[, which(colnames(df) == sprintf("gcc_%s", percentile))]

  # if the variability over the whole time series is too
  # small, no pattern will be detected, output empty data frame
  # this is a parameter
  #if (sd(raw_data, na.rm = TRUE) < 0.003) {
  #  cat("Too little seasonal variability, not reporting any values \n")
  #  return(err_function())
  #}

  # grab interpolation flags and put them
  # in a readable variable
  int_flag = df$int_flag
  
  # read in data based upon the parameters provided
  norm_data = normalize_ts(df, percentile = percentile)

  # Gcc values can't be within the range 0-1, causes
  # trouble with the changepoint algorithm
  # multiply with a set factor
  # normalize the time series first - to avoid giving more weight
  # to series with more amplitude.
  lower = norm_data$lower
  upper = norm_data$upper
  smooth = norm_data$smooth

  # put the dates in a separate vector
  date = as.Date(df$date)

  # switch the direction depending on the metric
  # needed (forward = rising edge, reverse = falling edge)
  if ( reverse == TRUE ) {
    lower = rev(lower)
    upper = rev(upper)
    int_flag = rev(int_flag)
    date = rev(date)
    smooth_orig = rev(smooth_orig)
    raw_data = rev(raw_data)
    smooth = rev(smooth)
  }

  cpt_obj = changepoint::cpt.mean(
    as.numeric(smooth[loc_freq]),
    method = 'PELT',
    test.stat = 'Normal',
    penalty = "Manual",
    pen.value = penalty,
    minseglen = seg_length,
    param.estimates = TRUE
  )

  index = 1:nrow(df)
  breakval = unlist(cpt_obj@param.est) # independent of location
  breaks = which(df$date %in% df$date[loc_freq][as.vector(cpt_obj@cpts)])

  # should delete high start values
  if (length(breaks) <= 1) {
    return(err_function())
  }

  # duplicate breaks to final pbreaks (will alter pbreaks, need the original breaks)
  pbreaks = breaks
  nbreaks = breaks

  # This routine trims out the breaks in the leading part of the season
  # only retaining the first greenup break
  for (i in 1:(length(breaks) - 1)) {
    if (i == 1) {
      # should delete high start values
      if (breakval[i] > breakval[i + 1]) {
        pbreaks[i] = NA
      }
    } else {
      # should delete values between true min and true max
      if (breakval[i] > breakval[i - 1] &
          breakval[i] < breakval[i + 1]) {
        pbreaks[i] = NA
      }

      # should delete max values
      if (breakval[i] > breakval[i - 1] &
          breakval[i] > breakval[i + 1]) {
        pbreaks[i] = NA
      }

      # should delete downward trends
      if (breakval[i] < breakval[i - 1] &
          breakval[i] > breakval[i + 1]) {
        pbreaks[i] = NA
      }
    }
  }

  # This routine trims out the breaks of all but the peaks
  for (i in 1:(length(breaks) - 1)) {
    if (i == 1) {
      # should delete high start values
      if (breakval[i] > breakval[i + 1]) {
        nbreaks[i] = NA
      }
    } else {
      # should delete values between true min and true max
      if (breakval[i] < breakval[i - 1] &
          breakval[i] < breakval[i + 1]) {
        nbreaks[i] = NA
      }

      # should delete values between true min and true max
      if (breakval[i] > breakval[i - 1] &
          breakval[i] < breakval[i + 1]) {
        nbreaks[i] = NA
      }
    }
  }

  # remove all NA values (trimming the breaks)
  pbreaks = pbreaks[!is.na(pbreaks)]
  nbreaks = nbreaks[!is.na(nbreaks)]
  nbreaks = unique(nbreaks)

  if (length(pbreaks) == 0) {
    return(err_function())
  }

  # now we now how many true leading edge breaks create matrices
  # to hold the final estimated dates
  # the final length of this matrix will be trimmed in the end
  # with a na.omit statement
  dates = as.data.frame(matrix(NA, length(pbreaks), 14))

  # for all breaks determine the inter-break values and calculate
  # the corresponding phenophase threshold values / dates for a
  # fraction of this inter-break section

  for (i in 1:(length(pbreaks))) {

    # loop over all segments
    # set begin and end of the segment, based upon the
    # fraction of the inter-break distance, use 1 for the
    # first segment, suppress warnings (will return NA if it fails)
    # NA statements are trapped below
    start = suppressWarnings(nbreaks[max(which(nbreaks < pbreaks[i]))])
    end = suppressWarnings(nbreaks[min(which(nbreaks > pbreaks[i]))])

    if (is.na(start)) {
      if (i == 1) {
        start = 1
      } else{
        next
      }
    }

    if (is.na(end)) {
      next
    }

    # cut the gcc vector into a segments using the above determined locations
    segment = smooth[start:end]
    segment_orig = smooth_orig[start:end]
    #solar_elev_segment = solar_elev[start:end]

    # store vector with the true locations
    index_segment = index[start:end]

    # grab the segment's interpolation flags
    int_flag_segment = int_flag[start:end]

    # subset of the raw data (not interpolated)
    raw_data_segment = raw_data[start:end]

    # grab the length of the longest interpolated stretch
    int_length = try(length(stats::na.contiguous(int_flag_segment)), silent = TRUE)
    if (inherits(int_length, "try-error")) {
      int_length = 0
    }

    # keep track of the absolute start point of the segment
    # -1 to start correctly after the start of the time series
    # if not enforced this will mess up thresholding later on
    # due to a miscount of 1 day (which has large consequences)
    # in rapidly changing time series
    offset = start - 1
    breakpoint = pbreaks[i]

    # solar elevation constraint on arctic sites
    # to prevent grabbing really early minima due to
    # skewed data
    loc = which(segment == min(segment[index_segment <= breakpoint]))
    
    # if no such point exists use the default (for most sites)
    if (length(loc) == 0 ){
      loc = which(segment == min(segment[index_segment <= breakpoint]))
    }

    min_loc = index_segment[loc]
    max_loc = index_segment[which(segment == max(segment[index_segment >= breakpoint]) )]

    # only use values before and after the breakpoint
    # to evaluate the quantiles to set boundaries for the amplitude
    # also limit the values on the minimum and maximum end for either
    # section: so low_loc = 1 / high_loc = 2
    # min loc - 1 - breakpoint loc - 2 - max loc
    low_loc = which(index_segment <= breakpoint & index_segment >= min_loc)
    high_loc = which(index_segment >= breakpoint & index_segment <= max_loc)

    # calculate segment amplitude (normalized)
    low_gcc = stats::quantile(segment[low_loc], 0.5, na.rm = TRUE)
    high_gcc = stats::quantile(segment[high_loc], 0.9, na.rm = TRUE)

    # original values to report in output
    # this step does not use interpolated data so it can lead
    # to discrepancies between the values used and those reported
    low_gcc_orig = stats::quantile(segment_orig[low_loc], 0.5 ,na.rm = TRUE)
    high_gcc_orig = stats::quantile(segment_orig[high_loc], 0.9 ,na.rm = TRUE)

    # calculate amplitude
    amplitude = high_gcc - low_gcc

    # calculate threshold values, based on amplitude
    minimum_threshold = amplitude * lower_thresh + low_gcc
    middle_threshold = amplitude * middle_thresh + low_gcc
    maximum_threshold = amplitude * upper_thresh + low_gcc

    # grab the end of season estimate (location in the vector)
    # offset corrects for the lack of true dates in the segment data
    # and as such outputs the location within the original sequence
    # the offset can be seen as a counter with uneven increments

    # end of season
    eos_loc = suppressWarnings(min(
      which(
        segment >= maximum_threshold &
          index_segment > min_loc &
          index_segment < max_loc
      )
    ) + offset)

    # middle of season
    mos_loc = suppressWarnings(max(
      which(
        segment <= middle_threshold &
          index_segment > min_loc &
          index_segment < max_loc &
          index_segment < eos_loc
      )
    ) + offset)

    # start of season
    sos_loc = suppressWarnings(max(
      which(
        segment <= minimum_threshold &
          index_segment > min_loc &
          index_segment < max_loc &
          index_segment < mos_loc
      )
    ) + offset)

    # if the sos and eos values are not infinite (void)
    # calculate the CI values

    if (is.infinite(sos_loc) | is.infinite(eos_loc)) {
      next
    } else {

      # clip the CI segments (these are Gcc values)
      lower_segment = lower[start:end]
      upper_segment = upper[start:end]

      # calculate transition dates
      # if the date is in an interpolated area use next valid values
      # otherwise use the upper and lower segment boundaries

      # end of the season
      if (!is.na(int_flag[eos_loc])) {

        eos_lower_loc = min(index[which(index >= eos_loc & is.na(int_flag))])
        eos_upper_loc = max(index[which(index <= eos_loc & is.na(int_flag))])

      } else {
        eos_lower_loc = min(index_segment[which(
          lower_segment >= maximum_threshold &
            index_segment > min_loc &
            index_segment < max_loc
        )])

        eos_upper_loc = max(index_segment[which(
          upper_segment <= maximum_threshold &
            index_segment > min_loc &
            index_segment < max_loc
        )])
      }

      # middle of the season
      if (!is.na(int_flag[mos_loc])) {

        mos_lower_loc = min(index[which(index >= mos_loc & is.na(int_flag))])
        mos_upper_loc = max(index[which(index <= mos_loc & is.na(int_flag))])

      } else {

        mos_lower_loc = min(index_segment[which(
          lower_segment >= middle_threshold &
            index_segment > min_loc &
            index_segment < max_loc
        )])

        mos_upper_loc = max(index_segment[which(
          upper_segment <= middle_threshold &
            index_segment > min_loc &
            index_segment < max_loc
        )])
      }

      # start of the season
      if (!is.na(int_flag[sos_loc])) {

        sos_lower_loc = min(index[which(index >= sos_loc & is.na(int_flag))])
        sos_upper_loc = max(index[which(index <= sos_loc & is.na(int_flag))])

      } else {

        sos_lower_loc = min(index_segment[which(
          lower_segment >= minimum_threshold &
            index_segment > min_loc &
            index_segment < max_loc
        )])

        sos_upper_loc = max(index_segment[which(
          upper_segment <= minimum_threshold &
            index_segment > min_loc &
            index_segment < max_loc
        )])
      }
    }

    # if CI values equal or are smaller than the main
    # value, reset to +_ 1 day
    eos_lower_loc = ifelse(eos_lower_loc <= eos_loc + 1, eos_loc + 1, eos_lower_loc)
    eos_upper_loc = ifelse(eos_upper_loc >= eos_loc - 1, eos_loc - 1, eos_upper_loc)

    sos_lower_loc = ifelse(sos_lower_loc <= sos_loc + 1, sos_loc + 1, sos_lower_loc)
    sos_upper_loc = ifelse(sos_upper_loc >= sos_loc - 1, sos_loc - 1, sos_upper_loc)

    mos_lower_loc = ifelse(mos_lower_loc <= mos_loc + 1, mos_loc + 1, mos_lower_loc)
    mos_upper_loc = ifelse(mos_upper_loc >= mos_loc - 1, mos_loc - 1, mos_upper_loc)

    # put the dates in a final matrix
    # first the normal dates
    dates[i, 1] = date[sos_loc]
    dates[i, 2] = date[mos_loc]
    dates[i, 3] = date[eos_loc]

    # then the lower CI estimates
    dates[i, 4] = date[sos_lower_loc]
    dates[i, 5] = date[mos_lower_loc]
    dates[i, 6] = date[eos_lower_loc]

    # then the upper CI estimates
    dates[i, 7] = date[sos_upper_loc]
    dates[i, 8] = date[mos_upper_loc]
    dates[i, 9] = date[eos_upper_loc]

    # export thresholds
    dates[i, 10] = smooth_orig[sos_loc]
    dates[i, 11] = smooth_orig[mos_loc]
    dates[i, 12] = smooth_orig[eos_loc]

    # export min max values
    dates[i, 13] = low_gcc_orig
    dates[i, 14] = high_gcc_orig

    # round values
    dates[i,10:14] = round(dates[i,10:14],5)

    # plot for debugging
    if (plot){

      segment_date = date[start:end]
      min_seg = segment_orig[low_loc]
      min_date = segment_date[low_loc]
      max_seg = segment_orig[high_loc]
      max_date = segment_date[high_loc]

      plot(as.Date(date),smooth_orig,
           type='l',
           col='grey',
           xlab='Date',
           ylab='Gcc')
      graphics::lines(as.Date(max_date),max_seg,col='blue',lty=2)
      graphics::lines(as.Date(min_date),min_seg,col='red',lty=2)

      graphics::segments(x0=as.Date(date[c(mos_lower_loc,eos_lower_loc,sos_lower_loc)]),
               x1=as.Date(date[c(mos_upper_loc,eos_upper_loc,sos_upper_loc)]),
               y0=smooth_orig[c(mos_loc,eos_loc, sos_loc)],
               y1=smooth_orig[c(mos_loc,eos_loc, sos_loc)],
               lwd=3
               )
      graphics::abline(v=as.Date(date[pbreaks]), lty = 2)
    }
  }

  # trim the rows which have only NA value (due to non valid segments)
  na_row = apply(dates, 1, function(x)
    all(is.na(x)))
  dates = dates[!na_row,]

  # if the matrix is empty, return NULL
  # for plotting purposes the default behaviour is to return
  # an empty matrix rather than NULL
  if (dim(dates)[1] == 0) {
    # inject an empty row to return a single row of NA values
    # multiple rows give problems on the plotting side (shiny server)
    dates[1,] = rep(NA, dim(dates)[2])
  }

  # switch order for a nicely formatted output
  dates = dates[,c(1,2,3,7,8,9,4,5,6,10,11,12,13,14)]

  colnames(dates) =
    c(
      sprintf("transition_%s",lower_thresh*100),
      sprintf("transition_%s",middle_thresh*100),
      sprintf("transition_%s",upper_thresh*100),
      sprintf("transition_%s_lower_ci",lower_thresh*100),
      sprintf("transition_%s_lower_ci",middle_thresh*100),
      sprintf("transition_%s_lower_ci",upper_thresh*100),
      sprintf("transition_%s_upper_ci",lower_thresh*100),
      sprintf("transition_%s_upper_ci",middle_thresh*100),
      sprintf("transition_%s_upper_ci",upper_thresh*100),
      sprintf("threshold_%s",lower_thresh*100),
      sprintf("threshold_%s",middle_thresh*100),
      sprintf("threshold_%s",upper_thresh*100),
      'min_gcc',
      'max_gcc'
    )

  # return the dates
  return(as.data.frame(dates))

}
