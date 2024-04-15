#' Compute DEA
#'
#' DEA analysis.
#'
#' @param data Dataset to analyse.
#' @param id A string with the DMU id or name variable.
#' @param input A character vector with input variables.
#' @param output A character vector with output variables.
#' @param rts Returns to scale.
#' \tabular{rlll}{
#'    \tab crs \tab\tab Constant returns to scale, convexity and free disposability. \cr
#'    \tab vrs \tab\tab Variable returns to scale, convexity and free disposability. \cr
#'    \tab drs \tab\tab Decreasing returns to scale, convexity, down-scaling and free disposability. \cr
#'    \tab irs \tab\tab Increasing returns to scale, (up-scaling, but not down-scaling), convexity and free disposability.
#' }
#' @param orientation Model orientation.
#' @param super If `TRUE` super efficiency scores are calculated.
#' @param scale If `TRUE` scale efficiency scores are calculated.
#' @param slack If `TRUE` slack values are calculated.
#' @param peers If `TRUE` peers are added to the response.
#' @return list
#' @export
compute_dea <- function(
    data,
    id,
    input,
    output,
    rts = c('crs', 'vrs', 'drs', 'irs'),
    orientation = c('in', 'out'),
    super = FALSE,
    scale = FALSE,
    slack = FALSE,
    peers = FALSE) {

  rts <- match.arg(rts)
  orientation <- match.arg(orientation)

  # Prepare and validate input
  x <- as.matrix(data[input])
  y <- as.matrix(data[output])
  ids <- data[[id]]
  check_data(x, y)

  # Calculate DEA metrics
  res <- compute_dea_(
    x, y, ids,
    rts = rts,
    orientation = orientation,
    super = super,
    scale = scale,
    slack = slack,
    peers = peers
  )

  res

}

#' compute_dea_ (internal helper)
#' @inheritParams compute_dea
#' @inheritParams compute_efficiency
#' @return list
#' @noRd
compute_dea_ <- function(x, y, ids, rts, orientation, super, scale, slack, peers) {

  # Set initial values
  super_res <- NULL
  slack_res <- NULL
  peers_res <- NULL
  scale_res <- NULL

  eff_res <- compute_efficiency(x, y, rts = rts, orientation = orientation)
  if (super) super_res <- compute_super_efficiency(x, y, rts = rts, orientation = orientation)
  if (scale) scale_res <- compute_scale_efficiency_internal(x, y, orientation = orientation)
  if (slack) slack_res <- compute_slack(x, y, eff_res)
  if (peers) {
    peers_res <- get_peers(eff_res, ids)
    peers_res <- peers_res[2:ncol(peers_res)]
  }

  res <- list(
    efficiency = eff_res$values,
    super_efficiency = super_res$values,
    scale_efficiency = scale_res$values,
    slack = slack_res$values
  )
  res <- do.call('cbind', res)
  if (peers) res <- cbind(res, peers_res)

  out <- structure(
    list(
      results = data.frame(dmu = ids, res),
      info = eff_res$info
    ), class = 'pioneer_dea'
  )

}
