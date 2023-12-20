#' @import shiny
#' @import bslib
#' @import ggplot2
#' @import haven
#' @import markdown
#' @import productivity
#' @import reactable
#' @import readxl
#' @import writexl
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
#' Note that pioneeR must be loaded into the namespace with `library(pioneeR)`
#' before you run the pioneeR app.
#'
#' You can load a data object in your current environment to the app. You can pass
#' a data frame or a character string with the object name of the data frame you
#' want to be loaded when the app launches. Note that you should only use data
#' frame objects. If you have a tibble (from the tidyverse) or a data table, you
#' can convert to an ordinary data.frame using \code{\link[base]{as.data.frame}}.
#'
#' @export
run_pioneer <- function(x = NULL, port = NULL, ...) {

  if (!is.null(x)) {
    set_local_data(x)
  }

  if (!is.null(port)) {
    port <- check_for_unsafe_port(port)
  }

  shiny::runApp(system.file('app', package = 'pioneeR'), port = port, ...)

}

#' @rdname run_pioneer
#' @export
runPioneeR <- run_pioneer

#' Unset environment variables
#'
#' Unsets the environment variables set by pioneeR
#'
#' @export
unset_env_vars <- \() Sys.unsetenv('PIONEER_DATA')
