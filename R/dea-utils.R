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
