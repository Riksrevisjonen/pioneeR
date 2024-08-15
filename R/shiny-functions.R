#' @import shiny
#' @import bslib
#' @import ggplot2
#' @import haven
#' @import markdown
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

  # Create app object and run app
  app <- shiny::shinyApp(ui, server, enableBookmarking = 'server')
  shiny::runApp(app, port = port, ...)

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

#' @param level Level of alert
#' @param message The message to show the user
#' @param object A reactive object to update
#' @param append Boolean. If the message should be appended to the reactive
#' @noRd
set_message <- function(level, message, object = NULL, append = FALSE) {
  classes <- switch(
    level,
    warning = "alert alert-warning",
    error = "alert alert-danger",
    "alert alert-info"
  )
  icon <- if (level == "info") "info-circle" else "exclamation-circle"
  div <- div(class = classes, list(bsicons::bs_icon(icon, class = "me-2"), message))
  # If we do not have a reactive, return the div
  if (is.null(object)) {
    return(div)
  }
  # If we have a reactive object, set or append our div to the reactive
  if (append) {
    current_tags <- object()
    object(tagList(current_tags, div))
  } else {
    object(div)
  }
  invisible()
}

#' Check if a function issues a warning message and catch it so we can send it to
#' the user
#' @noRd
catch_warnings <- function(expr, handler_expr, reactive, append = FALSE) {
  if (shiny::isRunning() || as.logical(Sys.getenv("PIONEER_SUPPRESS_WARNINGS", FALSE))) {
    withCallingHandlers(expr, warning = \(w) {
      msg <- if (inherits(w, "rlang_warning")) cli::ansi_strip(w$message) else conditionMessage(w)
      handler_expr("warning", msg, reactive)
      tryInvokeRestart("muffleWarning")
    })
  } else {
    expr
  }
}

#' Check if a function raises an error and catch it so that the app does not stop,
#' but the user is informed
#' @noRd
catch_exceptions <- function(expr, handler_expr, reactive, append = FALSE) {
  # We only want to catch errors if the app is running
  if (shiny::isRunning()) {
    tryCatch(catch_warnings(expr, handler_expr, reactive), error = \(e) {
      msg <- if (inherits(e, "rlang_error")) cli::ansi_strip(e$message) else conditionMessage(e)
      handler_expr("error", msg, reactive)
      NULL
    })
  } else {
    expr
  }
}
