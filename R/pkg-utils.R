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
  if (is.null(port)) return()
  port <- as.numeric(port)
  if (!port %in% 3000:65535 || port %in% unsafe_ports()) {
    msg <- 'A random port will be used instead'
    if (port %in% unsafe_ports()) {
      msg <- sprintf('Port {.strong {port}} is considered unsafe. %s', msg)
    } else {
      msg <- sprintf('Port number must be in the range 3000 through 65535. %s', msg)
    }
    cli::cli_warn(msg)
    port <- NULL
  }
  return(invisible(port))
}
