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

unsafe_ports <- function() {
  return(
    c(3659, 4045, 5060, 5061, 6000, 6566, 6665:6669, 6697)
  )
}

check_for_unsafe_port <- function(port) {
  if (is.null(port)) return()
  port <- as.numeric(port)
  if (port %in% unsafe_ports()) {
    cli::cli_warn(
      'Port {.strong {port}} is considered unsafe. A random port will be used instead.'
    )
    port <- NULL
  }
  return(invisible(port))
}
