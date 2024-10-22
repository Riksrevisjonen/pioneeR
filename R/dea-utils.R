#' Create a matrix for input or output variables
#'
#' Create a matrix for input or output variables that can be used in DEA models
#' from a supplied data.frame
#'
#' @param df A data.frame containing the data.
#' @param columns A character vector of column names to include in the matrix.
#' @param id A character string specifying the column with DMU IDs.
#' @param normalize A logical indicating whether to normalize the columns by their
#'   mean. Defaults to `FALSE`.
#'
#' @examples
#' df <- data.frame(id = 1:3, a = c(10, 20, 30), b = c(5, 15, 25))
#' create_matrix(df, columns = c("a", "b"), id = "id", normalize = TRUE)
#'
#' @return A matrix of inputs or outputs
#'
#' @export
create_matrix <- function(df, columns, id, normalize = FALSE) {
  if (!inherits(df, 'data.frame')) {
    cli::cli_abort('You must provide a valid data.frame')
  }
  if (!all(columns %in% colnames(df))) {
    cli::cli_abort('All column names must exist in the supplied data.frame')
  }
  if (!id %in% colnames(df)) {
    cli::cli_abort('ID column not found in the supplied data.frame')
  }
  x <- as.matrix(df[, columns])
  rownames(x) <- df[[id]]
  colnames(x) <- columns
  x <- if (normalize) apply(x, 2, FUN = \(x) { x / mean(x) }) else x
  x
}

#' Calculate scale efficiency
#'
#' Calculate scale efficiency from a set of inputs and outputs and return a
#' data.frame
#'
#' @param x A matrix of inputs, created with `create_matrix`
#' @param y A matrix of outputs, created with `create_matrix`
#' @param orientation `in` for input oriented models or `out` for output oriented
#' @param digits An integer with the number of digits to round to. If `NULL` the
#'   values are kept unrounded.
#'
#' @return A data frame containing the efficiency scores for CRS, VRS, the
#'   Scale Efficiency, the VRS to NIRS ratio, and a recommendation on whether to
#'   increase or decrease the size of the DMU.
#'
#' @examples
#' # Create matrices with random values
#' inputs <- matrix(runif(10, 1, 10), ncol = 2)
#' outputs <- matrix(runif(10, 1, 10), ncol = 2)
#' # Compute scale efficiency
#' res <- compute_scale_efficiency(
#'   inputs, outputs, orientation = 'out', digits = 2)
#' @export
compute_scale_efficiency <- function(
    x,
    y,
    orientation = c('in', 'out'),
    digits = NULL) {

  if (!is.matrix(x)) {
    cli::cli_abort('inputs must be a matrix of input values')
  }
  if (!is.matrix(y)) {
    cli::cli_abort('outputs must be a matrix of output values')
  }
  if (nrow(x) != nrow(y)) {
    len_x <- nrow(x)
    len_y <- nrow(y)
    cli::cli_abort(c(
      'The number of DMUs for inputs and outputs does not match',
      'i' = 'There {?is/are} {len_x} DMU{?s} in the input matrix, and {len_y} in the output matrix',
      'x' = 'The number of DMUs must match for the input and output matrices'
    ))
  }
  if (!is.null(digits) && !is.numeric(digits)) {
    cli::cli_warn(c(
      'The digits argument must be a numeric value or NULL. The argument will be ignored.',
      'i' = 'Expected NULL or numeric, got {digits} of type {typeof(digits)}',
      'x' = 'Argument digits must be a numeric or NULL'
    ))
    digits <- NULL
  }
  orientation <- match.arg(orientation)

  # Run DEA models
  crs_mod <- compute_efficiency(x, y, rts = 'crs', orientation = orientation, values_only = TRUE)$values
  vrs_mod <- compute_efficiency(x, y, rts = 'vrs', orientation = orientation, values_only = TRUE)$values
  nirs_mod <- compute_efficiency(x, y, rts = 'drs', orientation = orientation, values_only = TRUE)$values

  # If efficiency scores for a unit differs in the CRS and VRS models and the ratio
  # of the NIRS and VRS models is equal to 1, the unit should decrease its size. When
  # the CRS and VRS models differ, and the ratio of the NIRS and VRS models is *not*
  # equal to 1, the unit should increase its size.
  equal_crs_vrs <- round(crs_mod, 6L) == round(vrs_mod, 6L)
  optimal_scale <- ifelse(
    !equal_crs_vrs,
    ifelse(vrs_mod / nirs_mod == 1, 'Decrease', 'Increase'),
    '-'
  )

  out_mod <- data.frame(
    'CRS' = crs_mod,
    'VRS' = vrs_mod,
    'Scale.eff.' = crs_mod / vrs_mod,
    'VRS.NIRS.ratio' = vrs_mod / nirs_mod
  )

  if (!is.null(digits) || is.numeric(digits)) {
    out_mod <- round(out_mod, digits)
  }
  dmu_names <- if (is.null(rownames(x))) seq_len(nrow(x)) else rownames(x)
  out_mod <- cbind(
    data.frame('DMU' = dmu_names),
    out_mod,
    data.frame('Optimal.scale.size' = optimal_scale)
  )
  rownames(out_mod) <- NULL

  out_mod

}

#' check_data
#' @noRd
check_data <- function(x, y, xref = NULL, yref = NULL) {
  check_numeric(x, y)
  check_nunits(x, y)
  if (!is.null(xref) || !is.null(yref)) {
    if (ncol(xref) != ncol(x)) {
      msg <- c('Inconsistent number of input variables:' ,
               'x' = "You've supplied are {ncol(x)} column(s) for `x` and {ncol(xref)} column(s) for `xref`.")
      cli::cli_abort(msg)
    }
    if (ncol(yref) != ncol(y)) {
      msg <- c('Inconsistent number of output variables:' ,
               'x' = "You've supplied are {ncol(y)} column(s) for `y` and {ncol(yref)} column(s) for `yref`.")
      cli::cli_abort(msg)
    }
    check_numeric(xref, yref)
    check_nunits(xref, yref)
  }
}

#' check_data
#' @noRd
check_numeric <- function(x, y) {
  chk <- apply(cbind(x, y), 2, is.numeric)
  if (any(!chk)) {
    cli::cli_abort('All variables must be numeric.')
  }
}

#' check_data
#' @noRd
check_nunits <- function(x, y, ref = FALSE) {
  nx <- nrow(x)
  ny <- nrow(y)
  if (nx != ny) {
    if (ref) {
      msg <- "You've supplied {nrows(x)} rows for `xref` and {nrow(y)} column(s) for `yref`."
    } else {
      msg <- "You've supplied {nrows(x)} rows for `x` and {nrow(y)} column(s) for `y`."
    }
    cli::cli_abort(c('Inconsistent number of units: ', 'x' = msg))
  }
  if (nx < (ncol(x) * ncol(y)) || nx < (ncol(x) + ncol(y)) * 3) {
    cli::cli_warn(
      "The number of DMUs ({nx}) is lower than the recommended minimum. Consider adding more
      DMUs. You should have at least {max(c(ncol(x) * ncol(y), (ncol(x) + ncol(y)) * 3))}
      DMUs based on the number of inputs and outputs."
    )
  }
}

#' get an input or output matrix from a model object
#' @noRd
get_matrix_from_model <- function(model, type = c('input', 'output')) {
  if (!inherits(model, 'pioneer_model')) cli::cli_abort('Object must be of type pioneer_model')
  type = match.arg(type)
  as.matrix(model[attr(model, type)])
}

#' @export
print.pioneer_dea <- function(x, ...) {
  cat('Efficiency scores:\n')
  print(x$efficiency)
  invisible(x)
}

#' @export
summary.pioneer_dea <- function(object, ...) {
  cat(sprintf(
    'Technology is %s and %s oriented efficiency\n',
    toupper(attr(object$model, 'rts')),
    switch(attr(object$model, 'orientation'), 'in' = 'input', 'out' = 'output')
  ))
  cat(sprintf('Mean efficiency: %s\n', round(mean(object$efficiency), 4L)))
  cat('-----------\n')
  summary(object$efficiency)
}

#' @export
as.data.frame.pioneer_dea <- function(x, ...) {
  out <- list()
  if (!is.null(attr(x$model, 'dmu'))) x$dmu = attr(x$model, 'dmu')
  out$efficiency = x$efficiency
  if (!is.null(x$super_efficiency)) out$super_efficiency <- x$super_efficiency
  if (!is.null(x$slack)) out$slack <- x$slack
  if (!is.null(x$peers)) out <- c(out, x$peers)
  out <- c(out, x$model)
  structure(out, row.names = seq_len(dim(x$model)[1L]), class = 'data.frame')
}
