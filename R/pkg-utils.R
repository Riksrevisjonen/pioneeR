#' Create temporary data that can be used by pioneeR when run locally
#' @noRd
set_local_data <- function(x) {

  if (nzchar(Sys.getenv('PIONEER_DATA'))) {
    e <- Sys.getenv('PIONEER_DATA')
    cli::cli_abort(c(
      'Environment variable PINOEER_DATA must be empty.',
      'i' = 'Unset PINOEER_DATA before you run the app.',
      'x' = 'PIONEER_DATA is set with value {e}.'
    ))
  }

  # Convert tibble and data.table to data.frame
  if (inherits(x, 'tbl_df') || inherits(x, 'data.table')) x <- as.data.frame(x)

  # Convert a matrix to data.frame
  if (inherits(x, 'matrix')) x <- as.data.frame(x)

  if (inherits(x, 'data.frame')) {
    tmp <- tempfile()
    saveRDS(x, tmp)
    Sys.setenv('PIONEER_DATA' = tmp)
  } else {
    Sys.setenv('PIONEER_DATA' = '')
    cli::cli_warn('The supplied object cannot be converted to a data.frame.')
  }

  return(invisible())

}

#' Get list of unsafe ports that are used in Chrome
#' @noRd
unsafe_ports <- function() {
  return(
    c(3659, 4045, 5060, 5061, 6000, 6566, 6665:6669, 6697)
  )
}

#' Checks if the specified port is inside the ranges that are considered safe
#' @noRd
check_for_unsafe_port <- function(port) {
  if (is.null(port)) return(port)
  if (is.character(port)) {
    if (is.na(suppressWarnings(as.numeric(port)))) {
      port <- NULL
    } else {
      port <- as.numeric(port)
      return(check_for_unsafe_port(port))
    }
  } else if (is.list(port)) {
    port <- unlist(port)[1]
    return(check_for_unsafe_port(port))
  } else if (is.numeric(port)) {
    port <- as.integer(port)
    if (!port %in% 3000L:65535L || port %in% unsafe_ports()) {
      msg <- 'A random port will be used instead'
      if (port %in% unsafe_ports()) {
        msg <- sprintf('Port {.strong {port}} is considered unsafe. %s', msg)
      } else {
        msg <- sprintf('Port number must be in the range 3000 through 65535. %s', msg)
      }
      cli::cli_warn(msg)
      port <- NULL
    }
  }
  return(invisible(port))
}
