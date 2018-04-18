#' Starts the phenocamr shiny interface
#' 
#' The GUI allows you to interactively download
#' data and visualize time series.
#' 
#' @keywords GUI, front end, interactive
#' @export
#' @examples
#' 
#' \donttest{
#' # Starts the PhenoCam explorer GUI in a browser
#' phenocam_explorer()
#' }

phenocam_explorer = function(){
  appDir = sprintf("%s/shiny/phenocam_explorer",path.package("phenocamr"))
  suppressWarnings(shiny::runApp(appDir,
                display.mode = "normal",
                launch.browser = TRUE))
}