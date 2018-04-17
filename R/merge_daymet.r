#' Merge Daymet data with a PhenoCam data file
#' 
#' Combine PhenoCam time series with matching climatological variables from
#' Daymet.
#' 
#' @param data a PhenoCam data file or data structure
#' @param trim logical, trim the daymet data to the length of the
#' PhenoCam time series or include the whole Daymet time series (1980-current).
#' (default = \code{FALSE})
#' @param internal return a data structure if given a file on disk
#' (\code{TRUE} / \code{FALSE} = default)
#' @param out_dir output directory where to store data (default = tempdir())
#' @keywords time series, PhenoCam, Daymet, data integration
#' @export
#' @examples
#' 
#' \donttest{
#' # download demo data (do not smooth)
#' download_phenocam(site = "harvard$",
#'                   veg_type = "DB",
#'                   roi_id = "1",
#'                   frequency = "3")
#'
#' # merge data with daymet data
#' merge_daymet(paste0(tempdir(),"/harvard_DB_0001_3day.csv"))
#' }

merge_daymet  = function(data,
                         trim = FALSE,
                         internal = TRUE,
                         out_dir){
 
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
  
  # start and end year of daymet downloads
  start_yr = 1980
  end_yr = as.numeric(format(Sys.time(), "%Y")) - 1
  
  # Download all available daymet data
  daymet_status = try(daymetr::download_daymet(
    site = data$site,
    lat = data$lat,
    lon = data$lon,
    end = end_yr,
    internal = TRUE,
    silent = TRUE
  ),
  silent = TRUE
  )
    
  # error trap the latency in the Daymet data releases
  if(inherits(daymet_status,"try-error")){
    if (grepl("check coordinates", daymet_status)){
      
      # reset end year
      end_yr = end_yr - 1
      
      # download daymet data
      daymet_status = try(daymetr::download_daymet(
        site = data$site,
        lat = data$lat,
        lon = data$lon,
        end = end_yr,
        internal = TRUE,
        silent = TRUE
      ),
      silent = TRUE)
      
      if (inherits(daymet_status, "try-error")){
        stop(' Daymet data not available -- server issues / or location out of range') 
      }
    }
  }

  # read in daymet data
  daymet_data = daymet_status$data

  # create date strings
  daymet_dates = as.Date(sprintf("%s-%s",
                                 daymet_data$year,
                                 daymet_data$yday),
                         "%Y-%j")
  
  # read phenocam data
  phenocam_data = data$data
  
  # create phenocam dates string
  phenocam_dates = as.Date(phenocam_data$date)
  
  # set year ranges
  if (trim == FALSE){
    min_range = min(daymet_dates)
    max_range = max(max(daymet_dates),
                    max(phenocam_dates))
  }else{
    min_range = min(phenocam_dates)
    max_range = max(phenocam_dates)
  }
  
  # create the output matrix
  all_dates = seq(as.Date(min_range), as.Date(max_range), "days")
  all_years = format(all_dates,"%Y")
  all_doys = format(all_dates,"%j")
  nr_dates = length(all_dates)
  
  # create daymet subset matrix
  daymet_data = daymet_data[,-c(1:2)]
  
  # find overlap between datasets / USE MERGE / CLEAN UP SECTION
  cor_daymet_dates = which(all_dates %in% daymet_dates)
  cor_phenocam_dates = which(all_dates %in% phenocam_dates)
  
  # create output matrix
  daymet_col = dim(daymet_data)[2]
  phenocam_col = dim(phenocam_data)[2]
  nr_cols = daymet_col + phenocam_col
  output_matrix = as.data.frame(matrix(NA,length(all_dates),nr_cols))
  
  # now fill up the matrix matching dates
  # clean up the code, this is messy with the references to the columns
  output_matrix[,1] = as.character(all_dates) # fill in date values
  output_matrix[,2] = all_years # fill in date values 
  output_matrix[,3] = all_doys
    
  output_matrix[cor_daymet_dates,4:(daymet_col+3)] = as.matrix(daymet_data[cor_daymet_dates,])
  output_matrix[cor_phenocam_dates,(daymet_col+4):(daymet_col+phenocam_col)] = as.matrix(phenocam_data[,4:(phenocam_col)])
  
  # extract the phenocam header
  phenocam_colnames = names(phenocam_data)
  phenocam_colnames = phenocam_colnames[4:length(phenocam_colnames)]
  
  # create matrix holding the column descriptions
  output_header_names = matrix(c("date","year","doy",
                                 "dayl","prcp",
                                 "srad","swe",
                                 "tmax","tmin",
                                 "vp",phenocam_colnames),
                               nrow=1,
                               byrow=TRUE)
  
  # set new column names
  colnames(output_matrix) = output_header_names
  
  # put data back into the data structure
  data$data = output_matrix
  
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