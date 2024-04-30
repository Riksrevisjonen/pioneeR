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
    c(3659L, 4045L, 5060L, 5061L, 6000L, 6566L, 6665L:6669L, 6697L)
  )
}

#' Get a random safe port. We do not want to use the full range, so we cap at 9999
#' @noRd
get_safe_port <- function() {
  port_range <- 3000L:9999L
  port_range <- port_range[!port_range %in% unsafe_ports()]
  sample(port_range, 1)
}

#' Checks if the specified port is inside the ranges that are considered safe
#' @noRd
check_for_unsafe_port <- function(port) {
  if (is.null(port) || is.na(suppressWarnings(as.numeric(port)))) {
    port <- get_safe_port()
  } else if (!port %in% 3000:65535 || port %in% unsafe_ports()) {
    msg <- 'A random port will be used instead'
    if (port %in% unsafe_ports()) {
      msg <- sprintf('Port {.strong {port}} is considered unsafe. %s', msg)
    } else {
      msg <- sprintf('Port number must be in the range 3000 through 65535. %s', msg)
    }
    cli::cli_warn(msg)
    port <- get_safe_port()
  }
  return(invisible(port))
}
