#' Write a nested data structure of class phenocamr to file, reconstructing
#' the original data structure from included headers and data components.
#' 
#' @param df a nested data structure of class phenocamr
#' @param out_dir output directory where to store data 
#' @keywords time series, smoothing, phenocam
#' @export
#' @examples
#'
#' \donttest{
#' # with defaults, outputting a data frame
#' # with smoothed values, overwriting the original
#'
#' # download demo data (do not smooth)
#' download_phenocam(site = "harvard$",
#'                   veg_type = "DB",
#'                   roi_id = "1",
#'                   frequency = "3",
#'                   smooth = FALSE)
#'
#' # read the phenocamo data file
#' df = read_phenocam(paste0(tempdir(),"/harvard_DB_0001_3day.csv"))
#' 
#' # print data structure
#' print(str(df))
#' 
#' # write the phenocamo data file
#' write_phenocam(df, out_dir = tempdir())
#' }

write_phenocam <- function(df = NULL,
                           out_dir = tempdir()){
  
  if(class(df)!="phenocamr" | is.null(df)){
    stop("not a phenocamr dataset or no dataset provided")
  }
  
  # format filename
  filename = sprintf("%s/%s_%s_%s_%s.csv",
                     path.expand(out_dir),
                     df$site,
                     df$veg_type,
                     df$roi_id,
                     df$frequency)
  
  # collapse named vector into a matrix
  header = apply(cbind(names(df$header), df$header),
                 1,
                 function(x)paste(x,collapse=": "))
  
  # fix collated empty lines
  header = gsub(": NA", "", header)
  
  # writing the final data frame to file, retaining the original header
  utils::write.table(
    header,
    filename,
    quote = FALSE,
    row.names = FALSE,
    col.names = FALSE,
    sep = ""
  )
  suppressWarnings(utils::write.table(
    df$data,
    filename,
    quote = FALSE,
    row.names = FALSE,
    col.names = TRUE,
    sep = ",",
    append = TRUE
  ))
}