#' Compute efficiency
#'
#' Compute DEA efficiency scores.
#'
#' @inheritParams compute_dea
#' @param x A matrix with input values.
#' @param y A matrix with output values.
#' @param xref A matrix with input values for the reference technology. Defaults to `NULL`.
#' @param yref A matrix with output values for the reference technology. Defaults to `NULL`.
#' @param values_only Only return the calculated values.
#' @return list
#' @noRd
compute_efficiency <- function(
    x, y,
    xref = NULL,
    yref = NULL,
    rts,
    orientation,
    values_only = FALSE) {
  # Get dims
  dims <- get_dims(x, y, xref, yref, rts = rts, slack = FALSE)
  # Create linear program model
  lp <- create_lp(
    x = x, y = y,
    xref = xref, yref = yref,
    rts = rts,
    orientation = orientation,
    m = dims$n_inputs,
    n = dims$n_outputs,
    n_units = dims$n_units,
    n_constraints = dims$n_constraints,
    n_vars = dims$n_vars,
    slack = FALSE)
  # Solve model
  res <- solve_lp(
    lp, x, y,
    orientation = orientation,
    m = dims$n_inputs,
    n = dims$n_outputs)
  # Adjust values
  res <- adjust_values(res$values, res$lambda, res$eps)
  if (values_only) {
    res <- list(values = res$values)
  }

  res
}

#' Compute super efficiency
#'
#' Compute DEA super efficiency scores.
#'
#' @inheritParams compute_efficiency
#' @noRd
compute_super_efficiency <- function(
    x, y,
    rts,
    orientation) {
  dims <- get_dims(x, y, rts = rts, slack = FALSE)
  lambda <- matrix(0, dims$n_units, dims$n_units)
  values <- rep(NA_real_, dims$n_units)
  for (i in seq_len(dims$n_units)) {
    # Create linear program model
    lp <- create_lp(
      x[-i,,drop=FALSE],
      y[-i,,drop=FALSE],
      rts = rts,
      orientation = orientation,
      m = dims$n_inputs,
      n = dims$n_outputs,
      n_units = dims$n_units - 1,
      n_constraints = dims$n_constraints,
      n_vars = dims$n_vars - 1,
      slack = FALSE)
    # Solve model
    tmp <- solve_lp_single(
      x[i,,drop=FALSE],
      y[i,,drop=FALSE],
      lp = lp,
      orientation = orientation,
      m = dims$n_inputs,
      n = dims$n_outputs)[[1]]
    values[i] <- tmp$value
    lambda[i,-i] <- tmp$solution
  }
  eps <- lpSolveAPI::lp.control(lp)$epsilon['epsint']
  res <- adjust_values(values, lambda, eps)

  res
}

#' Compute slack
#'
#' Compute slack for a DEA efficiency analysis.
#'
#' @inheritParams compute_efficiency
#' @param model Output of `compute_efficiency()`.
#' @return list
#' @noRd
compute_slack <- function(x, y, values, rts, orientation) {
  # Get dims
  dims <- get_dims(x, y, rts = rts, slack = TRUE)
  # Scale inputs and outputs if needed
  scale <- scale_vars(
    x, y,
    m = dims$n_inputs,
    n = dims$n_outputs,
    n_units = dims$n_units)
  # Create linear program model
  lp <- create_lp(
    scale$x, scale$y,
    rts = rts,
    orientation = orientation,
    m = dims$n_inputs,
    n = dims$n_outputs,
    n_units = dims$n_units,
    n_constraints = dims$n_constraints,
    n_vars = dims$n_vars,
    n_lambda = dims$n_lambda,
    slack = TRUE)
  # Solve model
  res <- solve_lp_slack(
    lp,
    scale$x, scale$y,
    values = values, # slack is calculated based on unadjusted eff.scores
    orientation = orientation,
    m = dims$n_inputs,
    n = dims$n_outputs,
    n_units = dims$n_units,
    n_vars = dims$n_vars)
  # Adjust values
  res <- adjust_slack(
    lp,
    values = res$values,
    sx = res$sx,
    sy = res$sy,
    lambda = res$lambda,
    scaling = scale,
    m = dims$n_inputs,
    n = dims$n_outputs,
    n_units = dims$n_units)

  res
}

#' Get peers
#'
#' Get peers for each DMU.
#'
#' @param lambda Output of `compute_efficiency()$lambda`.
#' @param ids A vector with DMU names or ids.
#' @param threshold Minimum weight for extracted peers. Defaults to 0.
#' @return data.frame
#' @noRd
get_peers <- function(lambda, ids, threshold = 0) {
  ids <- if (is.null(ids)) seq_len(dim(lambda)[1L]) else ids
  pt_ <- lapply(seq_len(nrow(lambda)), \(i) which(lambda[i,] > threshold))
  maxp <- max(lengths(pt_))
  if (is.na(maxp) || maxp == 0) maxp <- 1
  bench <- t(vapply(pt_, \(x) x[1:maxp], numeric(maxp)))
  bench <- matrix(ids[bench], nrow = nrow(bench))
  colnames(bench) <- paste0('peer', 1:ncol(bench))
  bench <- data.frame(cbind(dmu = ids, bench))
  bench
}
