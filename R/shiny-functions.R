#' Run pioneeR
#'
#' Run the pioneeR app on your local machine.
#'
#' @importFrom shiny runApp
#'
#' @export
runPioneeR <- function() {

  runApp(system.file('app', package = 'pioneeR'))

}
