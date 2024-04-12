#' Create a matrix for input or output variables
#'
#' Create a matrix for input or output variables that can be used in DEA models
#' from a supplied data.frame
#'
#' @param df A data.frame
#' @param columns A vector of column names that should be included in the matrix
#' @param id The name of the column with the DMU IDs
#' @param normalize If `TRUE`, all columns will be normalized with a mean of 1
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' @export
create_matrix <- function(df, columns, id, normalize = FALSE) {
  lifecycle::deprecate_warn(
    '0.4.0', 'create_matrix()',
    details = 'create_matrix() is no longer needed and will be removed in the next release.')
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
  rownames(x) <- as.vector(df[, id])
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
  crs_mod <- compute_efficiency(x, y, type = 'crs', orientation = orientation, values_only = TRUE)$values
  vrs_mod <- compute_efficiency(x, y, type = 'vrs', orientation = orientation, values_only = TRUE)$values
  nirs_mod <- compute_efficiency(x, y, type = 'drs', orientation = orientation, values_only = TRUE)$values

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
}

#' create_dea_output
#' @noRd
create_dea_output <- function(res, type, orientation, dims, values_only) {
  if (values_only) {
    res <- list(values = res$values)
  } else {
    res$info <- create_model_info(type, orientation, dims)
  }
  res
}

#' create_model_info
#' @noRd
create_model_info <- function(type, orientation, dims) {
  list(
    type = type,
    orientation = orientation,
    dims = dims
  )
}

#' round_numeric
#' @noRd
round_numeric <- function(df, digits = 4L) {
  df[] <- lapply(df, function(x) if(is.numeric(x)) round(x, digits) else x)
  df
}

#' pioneer_dea print method
#' @noRd
print.pioneer_dea <- function(x, ...) {
  cat("DEA result:\n")
  # x$results <- round_numeric(x$results, 4L)
  utils::str(x)
}
