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
#' @param slack If `TRUE` slack values are calculated.
#' @param peers If `TRUE` peers are added to the response.
#' @return A list of class `pioneer_dea`
#' @export
compute_dea <- function(
    data,
    id,
    input,
    output,
    rts = c('crs', 'vrs', 'drs', 'irs'),
    orientation = c('in', 'out'),
    super = FALSE,
    slack = FALSE,
    peers = FALSE) {

  rts <- match.arg(rts)
  orientation <- match.arg(orientation)

  # Create model object
  model <- create_model_object(data, id, input, output, rts, orientation, model_type = 'dea')

  # Calculate DEA metrics
  compute_dea_(model, super, slack, peers)

}

#' create_model_object
#' @noRd
create_model_object <- function(
    data, id, input, output, rts, orientation, time,
    model_type = c('dea', 'malmquist')) {

  model_type <- match.arg(model_type)
  model <- cbind(data[input], data[output])
  attr(model, 'model') <- model_type
  attr(model, 'input') <- input
  attr(model, 'output') <- output
  attr(model, 'dmu') <- data[[id]]
  if (model_type == 'malm') {
    attr(model, 'time') <- data[[time]]
  }
  attr(model, 'rts') <- rts
  attr(model, 'orientation') <- orientation
  structure(model, class = 'pioneer_model')

}

#' compute_dea_ (internal helper)
#' @inheritParams compute_dea
#' @inheritParams compute_efficiency
#' @return list
#' @noRd
compute_dea_ <- function(model, super, slack, peers) {

  x <- get_matrix_from_model(model, 'input')
  y <- get_matrix_from_model(model, 'output')

  check_data(x, y)

  # Set initial values
  super_res <- slack_res <- peers_res <- NULL

  eff_res <- compute_efficiency(x, y, rts = attr(model, 'rts'), orientation = attr(model, 'orientation'))
  if (super) super_res <- compute_super_efficiency(x, y, rts = attr(model, 'rts'), orientation = attr(model, 'orientation'))
  if (slack) slack_res <- compute_slack(x, y, eff_res)
  if (peers) peers_res <- get_peers(eff_res, attr(model, 'dmu'))[-1]

  res <- list(
    efficiency = as.vector(eff_res$values)
  )
  if (super) res$super_efficiency <- as.vector(super_res$values)
  if (slack) res$slack <- as.vector(slack_res$values)
  if (peers) res$peers <- peers_res
  res$model <- model

  structure(res, class = 'pioneer_dea')

}
