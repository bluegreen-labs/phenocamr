#' Starts the phenocamr shiny interface
#' in your default browser. The GUI
#' allows you to interactively download
#' data and visualize timeseries.
#' 
#' @keywords GUI, front end, interactive
#' @export
#' @examples
#' 
#' \dontrun{
#' # Starts the PhenoCam explorer GUI in a browser
#' phenocam_explorer()
#' }

phenocam_explorer = function(){
  appDir = sprintf("%s/shiny/phenocam_explorer",path.package("phenocamr"))
  suppressWarnings(shiny::runApp(appDir,
                display.mode = "normal",
                launch.browser = TRUE))
}