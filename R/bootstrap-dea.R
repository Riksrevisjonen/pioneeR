#' @importFrom stats complete.cases median quantile rnorm sd var
NULL

#' Bootstrap a DEA model
#'
#' Run bootstrap on a DEA model to estimate bias corrected efficiency scores and
#' confidence intervals.
#'
#' @param dea An object of class 'pioneer_dea' from `compute_dea()`.
#' @param alpha One minus the confidence level required. Default is 0.05.
#' @param bw_rule A string with the type of bandwidth rule to be used, or a number
#'   with the bandwidth parameter. See details.
#' @param iterations The number of bootstrap iterations to perform. Default is 2000.
#'
#' @details
#' In order to bootstrap a DEA model, you must first create a model object using the
#' `compute_dea()` function. Note that you currently can only bootstrap models using
#' constant or variable returns to scale (RTS). If you try to bootstrap a model using another
#' RTS, the bootstrap will fail with an error message.
#'
#' The bandwidth argument can be set to either `ucv` for unbiased cross validation,
#' `silverman` for the Silverman rule, or `scott` for the Scott rule. If you provide a
#' number, this will be used directly as the bandwidth parameter `h`. This can be useful
#' to replicate results where `h` is given, such as Simar & Wilson (1998). For most practical
#' applications of the bootstrap, the default value of unbiased cross validation is sensible.
#'
#' @return A list of class `pioneer_bootstrap`.
#' @examples
#' # Load example data
#' fare89 <- deaR::Electric_plants
#' # Estimate efficiency
#' mod <- compute_dea(
#'   data = fare89,
#'   input = c("Labor", "Fuel", "Capital"),
#'   output = "Output",
#'   id = "Plant",
#' )
#' # Run bootstrap. Reducing the number of iterations to save processing time
#' boot <- bootstrap_dea(mod, iterations = 100)
#' # Print results
#' print(boot)
#' # Get summary
#' summary(boot)
#' @seealso [compute_dea()]
#' @export
bootstrap_dea <- function(dea, alpha = 0.05, bw_rule = 'ucv', iterations = 2000) {

  if (!inherits(dea, 'pioneer_dea')) {
    msg <- 'You must provide an object of class pioneer_dea. Run compute_dea() first'
    cli::cli_abort(
      message = c(
        msg,
        'x' = 'You must provide an object of class pioneer_data'
      ))
  }

  rts <- attr(dea$model, 'rts')
  if (!rts %in% c('crs', 'vrs')) {
    msg <- 'Only constant returns to scale and variable returns to scale are supported'
    cli::cli_abort(
      message = c(
        msg,
        'x' = 'Only vrs and crs are supported in the rts argument',
        'i' = 'You supplied {rts}'
      ))
  }
  orientation <- attr(dea$model, 'orientation')
  theta <- dea$efficiency
  dmu <- attr(dea$model, 'dmu')
  x <- get_matrix_from_model(dea$model, 'input')
  y <- get_matrix_from_model(dea$model, 'output')
  # Calculate h to be used in the kernel density function
  h <- if (is.numeric(bw_rule)) bw_rule else bw_rule(theta, rule = bw_rule)
  # Perform bootstrap
  boot <- bootstrap_dea_(x, y, theta, rts, orientation, alpha, h, iterations)
  # Return bootstrap object
  new_pioneer_bootstrap(
    boot, dmu, alpha, list(h = h, bw_rule = bw_rule), iterations, rts, orientation
  )
}

#' Find bandwidth parameter h based on data and bandwidth rule
#' @noRd
bw_rule <- function(delta, rule = c('ucv', 'silverman', 'scott')) {
  rule <- match.arg(rule)
  # Values must be in range 1, Inf. Take inverse if values are in range 0, 1
  if (min(delta) < 1) {
    delta <- 1/delta
  }
  # Values == 1 are artifacts of DEA method and should be removed for h
  delta_m <- delta[delta > 1]
  delta_2m <- c(delta_m, 2 - delta_m)
  # See Daraio & Wilson (2007) for a discussion on methods for determining bandwidth
  h <- switch(rule,
    silverman = stats::bw.nrd0(delta_2m),
    scott = stats::bw.nrd(delta_2m),
    ucv = suppressWarnings({ h <- stats::bw.ucv(delta_2m) }),
    suppressWarnings({ h <- stats::bw.ucv(delta_2m) })
  )
  # See Daraio & Wilson (2007), p. 61, eq. 3.26
  h <- h * 2^(1/5) * (length(delta_m)/length(delta))^(1/5) * (sd(delta)/sd(delta_2m))
  h
}

#' Perform bootstrap sampling according to Simar & Wilson
#' @noRd
bootstrap_sample <- function(delta, h, n = length(delta)) {
  # Values must be in range 1, Inf. Take inverse if values are in range 0, 1
  if (min(delta) < 1) {
    delta <- 1/delta
  }
  delta_2m <- c(delta, 2 - delta)
  beta <- sample(delta_2m, n, replace = TRUE)
  # See Simar & Wilson (1998), eq. 4.20
  beta <- beta + h * rnorm(n)
  beta[beta < 1] <- 2 - beta[beta < 1]
  # See Simar & Wilson (1998), eq. 4.25
  beta <- mean(delta) + 1/sqrt(1 + h^2/var(delta)) * (beta - mean(delta))
  # Return beta values from bootstrap sample
  1/beta
}

#' Perform the actual bootstrap iteration
#' @noRd
perform_boot <- function(x, y, rts, orientation, h, theta) {
  beta <- bootstrap_sample(theta, h = h)
  if (orientation == 'in') {
    x_ref <- (theta / beta) * x
    boot <- compute_efficiency(x, y, x_ref, y, rts = rts, orientation = orientation)$values
  } else if (orientation == 'out') {
    beta <- 1 / beta
    y_ref <- (theta / beta) * y
    boot <- compute_efficiency(x, y, x, y_ref, rts = rts, orientation = orientation)$values
  }
  return(invisible(boot))
}

#' Internal function to perform the bootstrap. This is also used by server.R
#' @noRd
bootstrap_dea_ <- function(x, y, theta, rts, orientation, alpha, h, iterations) {
  # Initiate an empty matrix to store results from each bootstrap iteration
  boot <- matrix(NA_real_, nrow = length(theta), ncol = iterations)
  # If we are in a Shiny app, we inform the user of progress using withProgress
  if (shiny::isRunning()) {
    withProgress(message = 'Running', min = 0, max = iterations, value = 0, {
      for (i in seq_len(iterations)) {
        incProgress(1, detail = sprintf('Iteration %s', i))
        boot[, i] <- perform_boot(x, y, rts, orientation, h, theta)
      }
    })
    # If we are in interactive mode outside of a Shiny app, we inform the user with txtProgressBar
  } else if (interactive()) {
    pg <- utils::txtProgressBar(min = 0, max = iterations, style = 3)
    for (i in seq_len(iterations)) {
      utils::setTxtProgressBar(pg, i)
      boot[, i] <- perform_boot(x, y, rts, orientation, h, theta)
    }
    close(pg)
  } else {
    for (i in seq_len(iterations)) {
      boot[, i] <- perform_boot(x, y, rts, orientation, h, theta)
    }
  }
  # Correcting for bias and constructing CI based on SW98, eq. 2.17 and 2.18
  bias <- as.vector(rowMeans(boot, na.rm = TRUE) - theta)
  theta_tilde <- theta - bias
  theta_ci <- t(apply(boot, 1, quantile, probs = c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)) - (2 * bias)
  # Check for missingness
  is_missing <- apply(boot, 1, \(x) any(is.na(x)))
  # Return a list of values
  out <- list(
    efficiency = theta,
    bootstrap = boot,
    efficiency_bc = theta_tilde,
    bias = bias,
    conf_int = theta_ci,
    range = apply(theta_ci, 1, diff)
  )
}

#' Constructor for S3 class pioneer_bootstrap
#' @noRd
new_pioneer_bootstrap <- function(object, dmu, alpha, bandwidth, iterations, rts, orientation) {
  stopifnot(is.list(object))
  stopifnot(!is.null(object$bootstrap))
  if (!is.null(dmu)) attr(object$bootstrap, 'dmu') <- dmu
  attr(object$bootstrap, 'alpha') <- alpha
  attr(object$bootstrap, 'bandwidth') <- bandwidth
  attr(object$bootstrap, 'iterations') <- iterations
  attr(object$bootstrap, 'rts') <- rts
  attr(object$bootstrap, 'orientation') <- orientation
  structure(object, class = 'pioneer_bootstrap')
}

#' @export
summary.pioneer_bootstrap <- function(object, ...) {
  cat(sprintf(
    'Bootstrap with %s iterations of DEA model with technology %s and %s oriented efficieny\n',
    attr(object$bootstrap, 'iterations'),
    toupper(attr(object$bootstrap, 'rts')),
    switch(attr(object$bootstrap, 'orientation'), 'in' = 'input', 'out' = 'output')
  ))
  cat(sprintf(
    'Mean bias corrected efficiency: %s\n', round(mean(object$efficiency_bc), 4L)
  ))
  cat(sprintf('Mean bias: %s\n', round(mean(object$bias), 4L)))
  cat('-----------\n')
  summary(object$efficiency_bc)
}

#' @export
print.pioneer_bootstrap <- function(x, ...) {
  cat('Bias corrected efficiency scores:\n')
  print(x$efficiency_bc)
  invisible(x)
}

#' @export
as.data.frame.pioneer_bootstrap <- function(x, ...) {
  stopifnot(is.matrix(x$bootstrap))
  out <- list(
    if (!is.null(attr(x$bootstrap, 'dmu'))) dmu = attr(x$bootstrap, 'dmu'),
    efficiency = x$efficiency,
    bias = x$bias,
    bias_corrected = x$efficiency_bc,
    lower = as.vector(x$conf_int[, 1]),
    upper = as.vector(x$conf_int[, 2]),
    range = x$range
  )
  structure(out, row.names = seq_len(dim(x$bootstrap)[1L]), class = 'data.frame')
}
