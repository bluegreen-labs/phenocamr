#' Calculates transition dates on the upward
#' segments of a PhenoCam time series. This function
#' should not be run stand-alone. Use phenophases instead
#' The required input is a PhenoCam data file
#' with the smooth.ts() routine using the BCI
#' optimized smoothing parameter
#'
#' @param df: a PhenoCam data file or data frame
#' @param percentile: which index to use for the calculation
#' of transition dates
#' @param lower.thresh: the minimum threshold used
#' to determine the transition dates (default=0.2)
#' @param upper.thresh: the maximum threshold used
#'  to determine the transition dates (default=0.8)
#' @param reverse: flip the direction of processing
#' if TRUE you calculate the senescence phase of the season
#' @param plot: plot for debugging purposes
#' @keywords PhenoCam, transition dates, phenology, time series
#' @export
#' @examples
#' # with defaults, outputting a data frame of transition dates
#' # for both the smoothed time series as well as the CI on this
#' # smoothed time series
#' 
#' df = download.phenocam(site="bartlett",
#'                        type="DB",
#'                        roi="1",
#'                        frequency=3)
#' my_dates = transition.dates(df)
#'
#' # dates need to be converted to standard notation using
#' as.Date(my_dates)

# This function takes a data frame with smoothed gcc
# values and CI as input, this data is generated by
# the smooth.ts() routine

transition.dates = function(df,
                            lower.thresh = 0.1,
                            middle.thresh = 0.25,
                            upper.thresh = 0.5,
                            percentile = 90,
                            reverse = FALSE,
                            plot = FALSE) {
  
  # load the changepoint library needed
  # to divide the time series in segments
  # to be evaluated for seasonal dynamics
  require(changepoint, quietly = TRUE)
  
  # error function
  err_function = function() {
    
    err_mat = matrix(NA, 1, 12)
    
    # assign column names if not empty
      colnames(err_mat) = c(
        'gcc_value',
        'start',
        'middle',
        'end',
        'start_lower_ci',
        'middle_lower_ci',
        'end_lower_ci',
        'start_upper_ci',
        'middle_upper_ci',
        'end_upper_ci',
        'start_threshold',
        'middle_threshold',
        'end_threshold',
        'min_gcc',
        'max_gcc'
      )
    return(as.data.frame(err_mat))
  }
  
  # Detect changes in the mean
  # using the changepoint toolbox
  # I'll use these changepoints as a guide
  # to cut up the time series into seasons
  # This is way around issues with multiple growth
  # cycles and seasons crossing the year date marker (1 Jan.)
  # minimum segment length is 30 days
  #
  # Reverse the direction of the algorythm to
  # detect 'senescence' dynamics
  
  # multiplier to scale the time series outside
  # the 0-1 range. The higher the value the more
  # sensitive the changepoint detection algorithm
  
  # For the sake of consistency scale the original
  # series between 0 and 1 first (min to max),
  # otherwise low amplitude series will see a lower
  # sensitivity relative to high amplitude ones.
  
  multiplier = 5
  seg_length = 21
  
  # if the data is not a data frame, load
  # the file (assuming it is a phenocam)
  # summary file, otherwise rename the
  # input data to df
  if (!is.data.frame(df)) {
    if (file.exists(df)) {
      # read the original data
      df = read.table(df, header = T, sep = ',')
    } else{
      stop("not a valid PhenoCam data file")
    }
  }
  
  # check if smooth data is present, if not stop
  if (!any(grepl("smooth", colnames(df)))) {
    stop("No smoothed data in data frame, smooth data first using smooth.ts()")
  }
  
  # select original smooth data, needed to grab the
  # threshold values on the original scale for export
  smooth_orig = df[, which(colnames(df) == sprintf("smooth_gcc_%s", percentile))]
  
  # if all smooth values are NA, bail
  if ( all(is.na(smooth_orig)) ) {
    return(err_function())
  }
  
  # raw data values
  raw_data = df[, which(colnames(df) == sprintf("gcc_%s", percentile))]
  
  # if the variability over the whole time series is too
  # small, no pattern will be detected, output empty string
  if (sd(raw_data, na.rm = TRUE) < 0.005) {
    return(err_function())
  }
  
  # grab interpolation flags and put them
  # in a readable variable
  int_flag = df$int_flag
  
  # read in data based upon the parameters provided
  norm_data = normalize.ts(df, percentile = percentile)
  norm_data = norm_data * multiplier
  
  # Gcc values can't be within the range 0-1, causes
  # trouble with the changepoint algorithm (divisions somewhere)
  # multiply with a set factor
  # normalize the time series first - to avoid giving more weight
  # to series with more amplitude.
  lower = norm_data$lower
  upper = norm_data$upper
  smooth = norm_data$smooth
  
  # put the dates in a separate vector
  date = as.Date(df$date)
  
  if (reverse == FALSE) {
    cpt_obj = cpt.mean(
      as.numeric(smooth),
      method = 'PELT',
      test.stat = 'Normal',
      minseglen = seg_length,
      param.estimates = TRUE
    )
  } else{
    # reverse all input data
    lower = rev(lower)
    upper = rev(upper)
    int_flag = rev(int_flag)
    date = rev(date)
    smooth_orig = rev(smooth_orig)
    raw_data = rev(raw_data)
    cpt_obj = cpt.mean(
      as.numeric(rev(smooth)),
      method = 'PELT',
      test.stat = 'Normal',
      minseglen = seg_length,
      param.estimates = TRUE
    )
  }
  
  # deconstruct the changepoint object into
  # more readable variables
  index = 1:length(cpt_obj@data.set)
  gcc = cpt_obj@data.set
  breaks = as.vector(cpt_obj@cpts)
  breakval = unlist(cpt_obj@param.est)
  
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
    segment = gcc[start:end]
    segment_orig = smooth_orig[start:end]
    
    # store vector with the true locations
    index_segment = index[start:end]
    
    # grab the segment's interpolation flags
    int_flag_segment = int_flag[start:end]
    
    # subset of the raw data (not interpolated)
    raw_data_segment = raw_data[start:end]
    
    # grab the length of the longest interpolated stretch
    int_length = try(length(na.contiguous(int_flag_segment)), silent = TRUE)
    if (inherits(int_length, "try-error")) {
      int_length = 0
    }
    
    # keep track of the absolute start point of the segment
    offset = start
    breakpoint = pbreaks[i]
  
    low_loc = which(index_segment <= breakpoint &
                    is.na(int_flag_segment))
    
    high_loc = which(index_segment >= breakpoint &
                    is.na(int_flag_segment))
    
    # calculate segment amplitude
    low_gcc = quantile(segment[low_loc], 0.5, na.rm = TRUE)
    high_gcc = quantile(segment[high_loc], 0.9, na.rm = TRUE)
    
    # original values to report
    low_gcc_orig = quantile(segment_orig[low_loc], 0.5 ,na.rm = TRUE)
    high_gcc_orig = quantile(segment_orig[high_loc], 0.9 ,na.rm = TRUE)
    
    # calculate amplitude
    amplitude = high_gcc - low_gcc
    
    # calculate threshold values, based on amplitude
    minimum_threshold = amplitude * lower.thresh + low_gcc
    middle_threshold = amplitude * middle.thresh + low_gcc
    maximum_threshold = amplitude * upper.thresh + low_gcc
    
    # grab the end of season estimate (location in the vector)
    # offset corrects for the lack of true dates in the segment data
    # and as such outputs the location within the original sequence
    # the offset can be seen as a counter with uneven increments
    
    # use suppressWarnings() to limit warning output, 'errors' are trapped
    # below. If the result is infinite (empty condition) it will not be registered
    # and I skip to the next segment
  
    min_loc = index_segment[which(segment == min(segment[index_segment <= breakpoint]))]
    max_loc = index_segment[which(segment == max(segment[index_segment >= breakpoint]))]
      
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
      
      # check this section !!!
      
      # end of the season
      if (!is.na(int_flag[eos_loc])) {
        # find the min max values on either side of the
        # date
        eos_lower_loc = min(index_segment[which(index_segment >= eos_loc &
                                                  is.na(int_flag_segment))])
        eos_upper_loc = max(index_segment[which(index_segment <= eos_loc &
                                                  is.na(int_flag_segment))])
        
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
        mos_lower_loc = min(index_segment[which(index_segment >= mos_loc &
                                                  is.na(int_flag_segment))])
        mos_upper_loc = max(index_segment[which(index_segment <= mos_loc &
                                                  is.na(int_flag_segment))])        
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
        sos_lower_loc = min(index_segment[which(index_segment >= sos_loc &
                                                  is.na(int_flag_segment))])
        sos_upper_loc = max(index_segment[which(index_segment <= sos_loc &
                                                  is.na(int_flag_segment))])
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
    if (plot == TRUE){
      
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
      lines(as.Date(max_date),max_seg,col='blue',lty=2)
      lines(as.Date(min_date),min_seg,col='red',lty=2)
      
      segments(x0=as.Date(date[c(mos_lower_loc,eos_lower_loc,sos_lower_loc)]),
               x1=as.Date(date[c(mos_upper_loc,eos_upper_loc,sos_upper_loc)]),
               y0=smooth_orig[c(mos_loc,eos_loc, sos_loc)],
               y1=smooth_orig[c(mos_loc,eos_loc, sos_loc)],
               lwd=2
               )
      
      abline(h=low_gcc_orig)
      abline(h=high_gcc_orig)
      abline(v=as.Date(date[pbreaks]))
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
      sprintf("transition_%s",lower.thresh*100),
      sprintf("transition_%s",middle.thresh*100),
      sprintf("transition_%s",upper.thresh*100),
      sprintf("transition_%s_lower_ci",lower.thresh*100),
      sprintf("transition_%s_lower_ci",middle.thresh*100),
      sprintf("transition_%s_lower_ci",upper.thresh*100),  
      sprintf("transition_%s_upper_ci",lower.thresh*100),
      sprintf("transition_%s_upper_ci",middle.thresh*100),
      sprintf("transition_%s_upper_ci",upper.thresh*100),  
      sprintf("threshold_%s",lower.thresh*100),
      sprintf("threshold_%s",middle.thresh*100),
      sprintf("threshold_%s",upper.thresh*100),
      'min_gcc',
      'max_gcc'
    )
  
  # return the dates
  return(as.data.frame(dates))
  
}