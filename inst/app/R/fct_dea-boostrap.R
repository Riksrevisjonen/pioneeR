bw_rule <- function(delta, rule = 'cv', adjust = TRUE, type = NULL) {
  bw_rule <- 3
  if (is.character(rule)) {
    bw_rule <- switch(rule,
                      'silverman' = 1,
                      'scott' = 2,
                      'cv' = 3,
                      'sw98' = 4,
                      3
    )
    if (is.null(bw_rule)) {
      cli::cli_warn('Unknown bandwidth rule. Using default value instead')
    }
  } else if (is.numeric(rule)) {
    if (rule %in% 1:3) {
      bw_rule <- rule
    } else {
      cli::cli_warn('Unknown bandwidth rule. Using default value instead')
    }
  } else {
    cli::cli_warn('Unknown bandwidth rule. Using default value instead')
  }
  # Values must be in range 1, Inf. Take inverse if values are in range 0, 1
  if (min(delta) < 1) {
    delta <- 1/delta
  }
  # Values == 1 are artifacts of DEA method and should be removed for h
  delta_m <- delta[delta > 1]
  delta_2m <- c(delta_m, 2 - delta_m)
  if (bw_rule == 1) {
    h <- bw.nrd0(delta_2m)
  } else if (bw_rule == 2) {
    h <- bw.nrd(delta_2m)
  } else if (bw_rule == 3) {
    suppressWarnings({ h <- bw.ucv(delta_2m) })
  } else if (bw_rule == 4) {
    return(0.014)
  }
  if (adjust) {
    h <- h * 2^(1/5) * (length(delta_m)/length(delta))^(1/5) * (sd(delta)/sd(delta_2m))
  }
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
    boot[, i] <- Benchmarking::dea(x, y, RTS = rts, ORIENTATION = orientation, XREF = x_ref, FAST = TRUE)
  } else if (orientation == 'out') {
    beta <- 1 / beta
    y_ref <- (theta / beta) * y
    boot[, i] <- Benchmarking::dea(x, y, RTS = rts, ORIENTATION = orientation, YREF = y_ref, FAST = TRUE)
  }
  return(invisible(boot))
}

process_boot <- function(rts, orientation, h, alpha, theta, boot) {
  # Correcting for bias and constructing CI based on SW98, eq. 2.17 and 2.18
  bias <- as.vector(rowMeans(boot) - theta)
  theta_tilde <- theta - bias
  theta_ci <- t(apply(boot, 1, quantile, probs = c(alpha / 2, 1 - alpha / 2))) - (2 * bias)
  # Return data
  out <- list(
    eff = theta,
    bias = bias,
    eff_bc = theta_tilde,
    ci = theta_ci,
    tbl = data.frame(
      eff = theta,
      bias = bias,
      eff_bc = theta_tilde,
      lower = as.vector(theta_ci[, 1]),
      upper = as.vector(theta_ci[, 2])
    )
  )
  invisible(out)
}
