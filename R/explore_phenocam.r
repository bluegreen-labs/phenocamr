#' Starts the phenocamr shiny interface
#' in your default browser
#' 
#' @param none : calling the function loads the GUI
#' @keywords GUI, front end, interactive
#' @export
#' @examples
#' 
#' \dontrun{
#' # Starts the PhenoCam explorer in a browser
#' explore_phenocam()
#' }

explore_phenocam = function(){
  appDir = sprintf("%s/shiny/phenocam_explorer",path.package("phenocamr"))
  shiny::runApp(appDir, display.mode = "normal",launch.browser=TRUE)
}