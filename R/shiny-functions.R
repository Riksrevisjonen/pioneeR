#' @import shiny
NULL

#' Run pioneeR
#'
#' Run the pioneeR app on your local machine.
#'
#' @param x A data frame that should be loaded with the app. See details.
#' @param port The TCP port that the application should listen on.
#' @param ... Other arguments to send to \code{\link[shiny]{runApp}}
#'
#' @details
#' You can load a data object in your current environment to the app. You can pass
#' a data frame or a character string with the object name of the data frame you
#' want to be loaded when the app launches. Note that you should only use data
#' frame objects. If you have a tibble (from the tidyverse) or a data table, you
#' can convert to an ordinary data.frame using \code{\link[base]{as.data.frame}}.
#'
#' @importFrom shiny runApp
#'
#' @export
runPioneeR <- function(x = NULL, port = NULL, ...) {

  if (inherits(x, 'data.frame'))
    Sys.setenv('PIONEER_DATA' = deparse(substitute(x)))
  else if (inherits(x, 'character'))
    Sys.setenv('PIONEER_DATA' = x)
  else
    Sys.setenv('PIONEER_DATA' = '')

  runApp(system.file('app', package = 'pioneeR'), port = port)

}
