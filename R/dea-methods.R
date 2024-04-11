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
    type,
    orientation,
    values_only = FALSE) {
  # Get dims
  dims <- get_dims(x, y, xref, yref, type = type, slack = FALSE)
  # Create linear program model
  lp <- create_lp(
    x = x, y = y,
    xref = xref, yref = yref,
    type = type,
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
  # Return
  res <- create_dea_output(
    res,
    type = type,
    orientation = orientation,
    dims = dims,
    values_only = values_only)
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
    type,
    orientation) {
  dims <- get_dims(x, y, type = type, slack = FALSE)
  lambda <- matrix(0, dims$n_units, dims$n_units)
  values <- rep(NA_real_, dims$n_units)
  for (i in seq_len(dims$n_units)) {
    # Create linear program model
    lp <- create_lp(
      x[-i,,drop=FALSE],
      y[-i,,drop=FALSE],
      type = type,
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
  # Return
  res <- create_dea_output(
    res,
    type = type,
    orientation = orientation,
    dims = dims,
    values_only = FALSE)
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
compute_slack <- function(x, y, model) {
  # Get dims
  dims <- get_dims(x, y, type = model$info$type, slack = TRUE)
  # Scale inputs and outputs if needed
  scale <- scale_vars(
    x, y,
    m = dims$n_inputs,
    n = dims$n_outputs,
    n_units = dims$n_units)
  # Create linear program model
  lp <- create_lp(
    scale$x, scale$y,
    type = model$info$type,
    orientation = model$info$orientation,
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
    values = model$unadj_values, # slack is calculated based on unadjusted eff.scores
    orientation = model$info$orientation,
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
  # Return
  res <- create_dea_output(
    res,
    type = model$info$type,
    orientation = model$info$orientation,
    dims = dims,
    values_only = FALSE)
  res
}

#' Compute scale efficiency
#'
#' Compute DEA scale efficiency scores.
#'
#' @inheritParams compute_efficiency
#' @noRd
compute_scale_efficiency_internal <- function(x, y, orientation) {

  check_data(x, y)
  # orientation <- match.arg(orientation)

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

  list(
    values = crs_mod / vrs_mod, # scale efficiency
    data = data.frame(
      crs = crs_mod,
      vrs = vrs_mod,
      vrs_nirs_ratio = vrs_mod / nirs_mod,
      optimal_scale_size = optimal_scale
    )
  )
}

#' Get peers
#'
#' Get peers for each DMU.
#'
#' @param model Output of `compute_efficiency()`.
#' @param ids A vector with DMU names or ids.
#' @param threshold Minimum weight for extracted peers. Defaults to 0.
#' @return data.frame
#' @noRd
get_peers <- function(model, ids, threshold = 0) {
  lambda <- model$lambda
  pt_ <- apply(lambda, 1, function(x) {which(x > threshold)})
  if (dim(lambda)[1] == 1) pt_ <- list(c(pt_))  # Only 1 DMU
  bench <- t(mapply(function(x) x[1:max(sapply(pt_, length))], pt_))
  maxp <- max(sapply(pt_, length))
  if (maxp == 0 | is.na(maxp)) maxp <- 1
  dim(bench) <- c(dim(lambda)[1], maxp)
  bench <- matrix(ids[bench], nrow=nrow(bench))
  colnames(bench) <- paste0('peer', 1:ncol(bench))
  bench <- data.frame(cbind(dmu = ids, bench))
  bench
}
