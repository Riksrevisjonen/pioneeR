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

#' Function to add JS and CSS dependencies to the app
#' @noRd
pioneer_scripts <- function() {
  htmltools::htmlDependency(
    name = 'pioneer-assets',
    version = utils::packageVersion('pioneeR'),
    package = 'pioneeR',
    src = 'www',
    script = 'pioneer.js',
    style = 'style.css'
  )
}

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

  port <- check_for_unsafe_port(port)

  pioneer_env <- new.env()
  environment(ui) <- pioneer_env
  environment(server) <- pioneer_env

  # shiny::runApp(system.file('app', package = 'pioneeR'), port = port, ...)
  shiny::shinyApp(
    ui,
    server,
    enableBookmarking = 'server',
    options = list(port = port),
    ...
  )

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

#' Check if time series data is balanced for use in Malmquist models
#' @noRd
check_balance <- function(data, id_var, time_var) {

  units <- unique(data[, id_var])
  time <- unique(data[, time_var])

  miss <- sapply(units, function(u) {
    unit_time <- unique(data[data[,id_var] == u, time_var])
    all(sapply(time, function(t) t %in% unit_time))
  })

  r <- list()

  if (!all(miss)) {
    r$data <- data[data[,id_var] %in% units[miss],]
    r$listwise <- TRUE
    r$message <- 'Data was not balanced, listwise deleting has been performed'
  } else {
    r$data <- data
    r$listwise <- FALSE
    r$message <- NULL
  }

  r

}
