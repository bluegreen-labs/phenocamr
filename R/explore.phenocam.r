#' Start the AmerifluxR shiny interface
#' @param ... none
#' @keywords GUI, front end, interactive
#' @export
#' @examples
#' 
#' # NOT RUN
#' # explore.phenocam()

explore.phenocam = function(){
  appDir = sprintf("%s/shiny/phenocam_explorer",path.package("phenocamr"))
  shiny::runApp(appDir, display.mode = "normal",launch.browser=TRUE)
}