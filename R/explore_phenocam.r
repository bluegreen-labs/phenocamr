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
#' explore_phenocam()
#' }

explore_phenocam = function(){
  appDir = sprintf("%s/shiny/phenocam_explorer",path.package("phenocamr"))
  shiny::runApp(appDir,
                display.mode = "normal",
                launch.browser = TRUE)
}