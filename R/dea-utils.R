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
    digits = NULL)
{

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
  crs_mod <- Benchmarking::dea(x, y, RTS = 'crs', ORIENTATION = orientation)
  vrs_mod <- Benchmarking::dea(x, y, RTS = 'vrs', ORIENTATION = orientation)
  nirs_mod <- Benchmarking::dea(x, y, RTS = 'drs', ORIENTATION = orientation)

  # If efficiency scores for a unit differs in the CRS and VRS models and the ratio
  # of the NIRS and VRS models is equal to 1, the unit should decrease its size. When
  # the CRS and VRS models differ, and the ratio of the NIRS and VRS models is *not*
  # equal to 1, the unit should increase its size.
  equal_crs_vrs <- round(crs_mod$eff, 6L) == round(vrs_mod$eff, 6L)
  optimal_scale <- ifelse(
    !equal_crs_vrs,
    ifelse(vrs_mod$eff / nirs_mod$eff == 1, 'Decrease', 'Increase'),
    '-'
  )

  out_mod <- data.frame(
    'CRS' = crs_mod$eff,
    'VRS' = vrs_mod$eff,
    'Scale.eff.' = crs_mod$eff / vrs_mod$eff,
    'VRS.NIRS.ratio' = vrs_mod$eff / nirs_mod$eff
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
