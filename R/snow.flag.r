#' Flags snowy pictures based upon Citizen Science analysis
#' of the select imagery (zooniverse). Will be included in dataset but
#' currently does not serve any real purpose.
#'
#' @param df: a PhenoCam data file or data frame
#' @keywords PhenoCam, pre-processing, screening, snow, outliers
#' @export
#' @examples
#' # This is an internal function only.

snow.flag = function(df) {
  # read file if not a data frame
  if (!is.data.frame(df)) {
    phenocam_data = read.table(df, header = T, sep = ',')
  }
  
  # include this in the data part of the package, good for now
  snow_data = read.table(
    '/data/Dropbox/Research_Projects/working/FOV_crunch/snow_data_final.csv',
    sep = ',',
    header = T
  )
  
  # extract the site name from an image filename
  mysite = unlist(lapply(strsplit(
    as.character(phenocam_data$midday_filename), "_"
  )[1], "[[", 1))
  
  # subset site snow data
  snow_subset = subset(snow_data,
                       site == mysite,
                       select = c("date", "snow_majority_text"))
  
  # extract date
  snow_subset_dates = as.Date(snow_subset$date, "%Y_%m_%d")
  
  # get image dates
  phenocam_dates = as.Date(df$date, "%Y-%m-%d")
  
  for (k in ss_dates) {
    value = snow_subset$snow_majority_text[which(snow_subset_dates %in% k)]
    phenocam_data$snow_flag[which(phenocam_dates %in% k)] = value
  }
  
  if (is.data.frame(df)) {
    return(phenocam_data)
  } else{
    # get colnames
    phenocam_colnames = names(phenocam_data)
    phenocam_colnames = matrix(phenocam_colnames, 1, length(phenocam_colnames))
    
    # pluck real header from the phenocam file
    phenocam_header = readLines(df, n = 22)
    
    # create output filename string
    output_file_name = sprintf("%s.csv", unlist(strsplit(df, ".csv")))
    
    # write everything to file using append
    write.table(
      phenocam_header,
      output_file_name,
      quote = F,
      row.names = F,
      col.names = F,
      sep = ","
    )
    
    # check if this is still necessary if I flag the col.names=T
    # in the final data write.
    write.table(
      phenocam_colnames,
      output_file_name,
      quote = F,
      row.names = F,
      col.names = F,
      append = TRUE,
      sep = ","
    )
    write.table(
      phenocam_data,
      output_file_name,
      quote = F,
      row.names = F,
      col.names = F,
      append = TRUE,
      sep = ","
    )
  }
}