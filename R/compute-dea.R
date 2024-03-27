#' Compute efficiency
#'
#' Calculate DEA efficiency scores.
#'
#' @param x A matrix with input values.
#' @param y A matrix with output values.
#' @param xref A matrix with input values for the reference technology.
#' @param yref A matrix with output values for the reference technology.
#' @param type Type of model.
#' \tabular{rlll}{
#'    \tab vrs \tab\tab Variable returns to scale, convexity and free disposability. \cr
#'    \tab drs \tab\tab Decreasing returns to scale, convexity, down-scaling and free disposability. \cr
#'    \tab crs \tab\tab Constant returns to scale, convexity and free disposability. \cr
#'    \tab irs \tab\tab Increasing returns to scale, (up-scaling, but not down-scaling), convexity and free disposability.
#' }
#' @param orientation Model orientation.
#' @param digits Number of decimal digits in the response. Defaults to `NULL` (no rounding).
#' @return list
#' @export
compute_efficiency <- function(
    x, y,
    xref = NULL,
    yref = NULL,
    type = c('crs', 'vrs', 'drs', 'irs'),
    orientation = c('in', 'out'),
    digits = NULL) {
  type <- match.arg(type)
  orientation <- match.arg(orientation)
  check_data(x, y, xref, yref)
  # Get dimensions
  d <- get_dims(x, y, xref, yref, type = type, slack = FALSE)
  # Create linear program model
  lp <- create_lp(
    x = x, y = y,
    xref = xref, yref = yref,
    type = type,
    orientation = orientation,
    m = d$n_inputs,
    n = d$n_outputs,
    n_units = d$n_units,
    n_constraints = d$n_constraints,
    n_vars = d$n_vars,
    slack = FALSE)
  # Solve model
  res <- solve_lp(
    lp, x, y,
    orientation = orientation,
    m = d$n_inputs,
    n = d$n_outputs)
  # Adjust values
  res <- adjust_values(res$values, res$lambda, res$eps, digits)
  return(res)
}

#' Compute slack
#'
#' Calculate slack for a DEA efficiency analysis.
#'
#' @inheritParams compute_efficiency
#' @param values Efficiency values (normally unadjusted).
#' @return list
#' @export
compute_slack <- function(
    x, y,
    values,
    type = c('crs', 'vrs', 'drs', 'irs'), # 'fdh'
    orientation = c('in', 'out'),
    digits = NULL) {
  type <- match.arg(type)
  orientation <- match.arg(orientation)
  check_data(x, y)
  d <- get_dims(x, y, type = type, slack = TRUE)
  # Scale inputs and outputs if needed
  scale <- scale_vars(
    x, y,
    m = d$n_inputs,
    n = d$n_outputs,
    n_units = d$n_units)
  # Create linear program model
  lp <- create_lp(
    scale$x, scale$y,
    type = type,
    orientation = orientation,
    m = d$n_inputs,
    n = d$n_outputs,
    n_units = d$n_units,
    n_constraints = d$n_constraints,
    n_vars = d$n_vars,
    n_lambda = d$n_lambda,
    slack = TRUE)
  # # Solve model
  res <- solve_lp_slack(
    lp,
    scale$x, scale$y,
    values = values,
    orientation = orientation,
    m = d$n_inputs,
    n = d$n_outputs,
    n_units = d$n_units,
    n_vars = d$n_vars)
  # Adjust values
  res <- adjust_slack(
    lp,
    values = res$values,
    sx = res$sx,
    sy = res$sy,
    lambda = res$lambda,
    scaling = scale,
    m = d$n_inputs,
    n = d$n_outputs,
    n_units = d$n_units,
    digits = digits)
  return(res)
}

#' Compute super efficiency
#'
#' Calculate DEA super efficiency scores.
#'
#' @inheritParams compute_efficiency
#' @export
compute_super_efficiency <- function(
    x, y,
    type = c('crs', 'vrs', 'drs', 'irs'),
    orientation = c('in', 'out'),
    digits = NULL) {
  type <- match.arg(type)
  orientation <- match.arg(orientation)
  check_data(x, y)
  d <- get_dims(x, y, type = type, slack = FALSE)
  lambda <- matrix(0, d$n_units, d$n_units)
  values <- rep(NA_real_, d$n_units)
  for (i in seq_len(d$n_units)) {
    # Create linear program model
    lp <- create_lp(
      x[-i,,drop=FALSE],
      y[-i,,drop=FALSE],
      type = type,
      orientation = orientation,
      m = d$n_inputs,
      n = d$n_outputs,
      n_units = d$n_units - 1,
      n_constraints = d$n_constraints,
      n_vars = d$n_vars - 1,
      slack = FALSE)
    # Solve model
    tmp <- solve_lp_single(
      x[i,,drop=FALSE],
      y[i,,drop=FALSE],
      lp = lp,
      orientation = orientation,
      m = d$n_inputs,
      n = d$n_outputs)[[1]]
    values[i] <- tmp$value
    lambda[i,-i] <- tmp$solution
  }
  eps <- lpSolveAPI::lp.control(lp)$epsilon['epsint']
  res <- adjust_values(values, lambda, eps, digits)
  return(res)
}

#' Get peers
#'
#' Find peers for each DMU.
#'
#' @param lambda Output of `compute_efficiency()$lambda`.
#' @param ids A vector with DMU names or ids.
#' @param threshold Minimum weight for extracted peers. Defaults to 0.
#' @return data.frame
#' @export
get_peers <- function(lambda, ids, threshold=0) {
  pt_ <- apply(lambda, 1, function(x){which(x>threshold)})
  if (dim(lambda)[1] == 1) pt_ <- list(c(pt_))  # Only 1 DMU
  bench <- t(mapply(function(x) x[1:max(sapply(pt_, length))], pt_))
  maxp <- max(sapply(pt_, length))
  if (maxp==0 | is.na(maxp)) maxp <- 1
  dim(bench) <- c(dim(lambda)[1], maxp)
  bench <- matrix(ids[bench], nrow=nrow(bench))
  colnames(bench) <- paste0('peer', 1:ncol(bench))
  bench <- data.frame(cbind(dmu = ids, bench))
  return(bench)
}