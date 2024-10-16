#' Create a summary table for DEA
#'
#' Create a binned summary table for efficiency scores from a DEA model.
#'
#' @details
#' The function will return a summary table for efficiency scores from a DEA model.
#' Efficiency scores will be placed in 11 bins, where DMUs with an efficiency score
#' equal to 1 are placed in a separate bin. For output oriented models with range
#' \[1, Inf\], bins are created with `1/bin`. Bin widths will be equal to models
#' with range \[0, 1\].
#'
#' @param x A numeric vector of efficiency scores or an object of class `pioneer_dea`
#'
#' @return A [data.frame()] with summary statistics
#' @examples
#' # Load example data
#' fare89 <- deaR::Electric_plants
#' # Estimate efficiency
#' mod <- compute_dea(
#'   data = fare89,
#'   input = c("Labor", "Fuel", "Capital"),
#'   output = "Output",
#'   rts = "vrs"
#' )
#' # Get a summary table of efficiency scores
#' summary_tbl_dea(mod)
#' # You can also create the table from a numeric vector of efficiency scores
#' res <- as.data.frame(mod)
#' summary_tbl_dea(res$efficiency)
#' @export
summary_tbl_dea <- function(x) {
  UseMethod("summary_tbl_dea")
}

#' @method summary_tbl_dea pioneer_dea
#' @export
summary_tbl_dea.pioneer_dea <- function(x) {
  eff <- x$efficiency
  summary_tbl_dea(eff)
}

#' @method summary_tbl_dea numeric
#' @export
summary_tbl_dea.numeric <- function(x) {
  # Input validation
  if (!is.numeric(x) || length(x) == 0) {
    cli::cli_abort("Input must be a non-empty numeric vector")
  }
  # Remove missing
  x <- x[!is.na(x)]
  # Check if efficiency scores are in range [0, 1]
  range0 <- min(x) < 1L
  bins <- if (range0) seq(0, 1.1, .1) else round(1/rev(seq(0, 1.1, .1)), 3L)
  # Values equal to 1 be in last bin for input and first bin for output orientation
  # Create new labels to use with the cut function
  labs <- if (range0) {
    c(sprintf("%s <= E < %s", bins[1:10], bins[2:11]), "E == 1")
  } else {
    c("F == 1", sprintf("%.3f < F <= %.3f", bins[2:11], bins[3:12]))
  }
  # If efficiency scores are in range [0, 1] bins must be closed on the left
  eff_bin <- cut(x, breaks =  bins, labels = labs, right = !range0)
  eff_df <- table(eff_bin) |> as.data.frame()
  colnames(eff_df) <- c("Range", "Frequency")
  eff_df
}
