#' compute_mlm (pairwise)
#' @noRd
compute_mlm_pairwise <- function(dl, model) {
  n <- length(dl) - 1
  n_units <- length(unique(attr(model, 'dmu')))
  x <- create_mlm_matrix(dl, attr(model, 'input'))
  y <- create_mlm_matrix(dl, attr(model, 'output'))
  tmp <- vector('list', n)
  for(i in seq_len(n)) {
    tmp[[i]] <- compute_mlm_single(
      x0 = x[[i]], y0 = y[[i]],
      x1 = x[[i+1]], y1 = y[[i+1]],
      orientation = attr(model, 'orientation'))
  }
  get_mlm_values(tmp, n_units)
}

#' compute_mlm_single
#' Computations are based on definitions in Färe, R., Grosskopf, S. (1996).
#' _Intertemporal production frontiers: With dynamic DEA_. Springer.
#' @param x0 Inputs for first time period
#' @param y0 Outputs for first time period
#' @param x1 Inputs for second time period
#' @param y1 Outputs for second time period
#' @inheritParams compute_efficiency
#' @noRd
compute_mlm_single <- function(x0, y0, x1, y1, orientation) {

  check_data(x0, y0, x1, y1)

  c00 <- compute_efficiency(x0, y0, rts = 'crs', orientation = orientation, values_only = TRUE)$values # period 0
  c11 <- compute_efficiency(x1, y1, rts = 'crs', orientation = orientation, values_only = TRUE)$values # period 1

  c01 <- compute_efficiency(x0, y0, xref = x1, yref = y1, rts = 'crs', orientation = orientation, values_only = TRUE)$values # period 0 w/ period 1 as reference
  c10 <- compute_efficiency(x1, y1, xref = x0, yref = y0, rts = 'crs', orientation = orientation, values_only = TRUE)$values # period 1 w/ period 0 as reference

  c1000 <- compute_efficiency(x1, y0, xref = x0, yref = y0, rts = 'crs', orientation = orientation, values_only = TRUE)$values # x1 input vs y0 output, in tech t
  c1011 <- compute_efficiency(x1, y0, xref = x1, yref = y1, rts = 'crs', orientation = orientation, values_only = TRUE)$values # x1 input vs y0 output, in tech t+1

  v00 <- compute_efficiency(x0, y0, rts = 'vrs', orientation = orientation, values_only = TRUE)$values # period 0 (vrs)
  v11 <- compute_efficiency(x1, y1, rts = 'vrs', orientation = orientation, values_only = TRUE)$values # period 1 (vrs)

  effch <- c11 / c00                        # Efficiency change (catch-up relative to present technology), F&G p. 57 (3.2.13)
  tech <- sqrt((c10 / c11) * (c00 / c01))   # Technical change (change of frontier), F&G p. 57 (3.2.14)
  malmquist <- effch * tech                 # Malmquist index (change in productivity), F&G p. 55-57

  obtech <- sqrt((c10 / c11) * (c1011 / c1000))  # Output biased technical change, F&G p. 91 (4.2.3)
  ibtech <- sqrt((c01 / c00) * (c1000 / c1011))  # Input biased technical change, F&G p. 95 (4.2.10)
  matech <- c00 / c01                            # Magnitude component (magnitude of technical change), F&G p. 95 (4.2.12)

  scale_effch <- (c11 / c00) / (v11 / v00) # Scale efficiency change, F&G p. 66-67 (3.4.10, first ratio)
  pure_effch <- v11 / v00                  # Pure (technical) efficiency change, F&G p. 67 (3.4.10, second ratio)

  list(malmquist = malmquist,
       effch = effch,
       tech = tech,
       obtech = obtech,
       ibtech = ibtech,
       matech = matech,
       scale_effch = scale_effch,
       pure_effch = pure_effch)
}
