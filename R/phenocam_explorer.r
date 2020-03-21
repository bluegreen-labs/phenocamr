#' Starts the phenocamr shiny interface
#' 
#' The GUI allows you to interactively download
#' data and visualize time series.
#' 
#' @keywords GUI interactive
#' @export
#' @examples
#' 
#' \dontrun{
#' # Starts the PhenoCam explorer GUI in a browser
#' phenocam_explorer()
#' }

phenocam_explorer = function(){
  if(!requireNamespace(c("DT",
                         "plotly",
                         "shinydashboard",
                         "leaflet"), quietly = TRUE)){
    stop("Packages \"DT, plotly, shinydashboard and leaflet\" are needed 
         for this function to work. Please install it.",
         call. = FALSE)
  }
  
  appDir = sprintf("%s/shiny/phenocam_explorer", path.package("phenocamr"))
  suppressWarnings(shiny::runApp(appDir,
                display.mode = "normal",
                launch.browser = TRUE))
}