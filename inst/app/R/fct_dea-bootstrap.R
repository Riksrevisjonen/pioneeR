bw_rule <- function(delta, rule = 'ucv') {
  # Values must be in range 1, Inf. Take inverse if values are in range 0, 1
  if (min(delta) < 1) {
    delta <- 1/delta
  }
  # Values == 1 are artifacts of DEA method and should be removed for h
  delta_m <- delta[delta > 1]
  delta_2m <- c(delta_m, 2 - delta_m)
  # See Daraio & Wilson (2007) for a discussion on methods for determining bandwidth
  h <- switch(rule,
    silverman = bw.nrd0(delta_2m),
    scott = bw.nrd(delta_2m),
    ucv = suppressWarnings({ h <- bw.ucv(delta_2m) }),
    suppressWarnings({ h <- bw.ucv(delta_2m) })
  )
  # See Daraio & Wilson (2007), p. 61, eq. 3.26
  h <- h * 2^(1/5) * (length(delta_m)/length(delta))^(1/5) * (sd(delta)/sd(delta_2m))
  h
}

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

perform_boot <- function(x, y, rts, orientation, i, h, theta, boot) {
  beta <- bootstrap_sample(theta, h = h)
  if (orientation == 'in') {
    x_ref <- (theta / beta) * x
    boot[, i] <- compute_efficiency(x, y, type = rts, orientation = orientation, xref = x_ref, yref = y, values_only = TRUE)$values
  } else if (orientation == 'out') {
    beta <- 1 / beta
    y_ref <- (theta / beta) * y
    boot[, i] <- compute_efficiency(x, y, type = rts, orientation = orientation, xref = x, yref = y_ref, values_only = TRUE)$values
  }
  return(invisible(boot))
}

process_boot <- function(rts, orientation, h, alpha, theta, boot) {
  # Correcting for bias and constructing CI based on SW98, eq. 2.17 and 2.18
  bias <- as.vector(rowMeans(boot, na.rm = TRUE) - theta)
  theta_tilde <- theta - bias
  theta_ci <- t(apply(boot, 1, quantile, probs = c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)) - (2 * bias)
  # Check for missingness
  is_missing <- apply(boot, 1, \(x) any(is.na(x)))

  # Return data
  out <- list(
    eff = theta,
    bias = bias,
    eff_bc = theta_tilde,
    ci = theta_ci,
    missing = if (all(!is_missing)) NULL else which(is_missing),
    tbl = data.frame(
      eff = theta,
      bias = bias,
      eff_bc = theta_tilde,
      lower = as.vector(theta_ci[, 1]),
      upper = as.vector(theta_ci[, 2]),
      range = apply(theta_ci, 1, diff)
    )
  )
  invisible(out)
}
