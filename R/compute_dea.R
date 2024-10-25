#' Compute DEA
#'
#' Solve an input or output oriented DEA model under constant (`crs`), variable (`vrs`),
#' non-increasing (`drs`), or non-decreasing (`irs`) returns to scale.
#'
#' @param data Dataset to analyse.
#' @param id Optional. A string with the DMU id or name variable. Defaults to the rownames of the dataset.
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
#' @return A list of class `pioneer_dea`.
#' @examples
#' # Load example data
#' fare89 <- deaR::Electric_plants
#' # Estimate efficiency
#' mod <- compute_dea(
#'   data = fare89,
#'   input = c("Labor", "Fuel", "Capital"),
#'   output = "Output",
#'   id = "Plant",
#'   rts = "vrs",
#'   orientation = "in"
#' )
#' # Print results
#' print(mod)
#' # Get summary
#' summary(mod)
#' # Convert to data frame
#' df <- as.data.frame(mod)
#' @export
compute_dea <- function(
    data,
    input,
    output,
    id = NULL,
    rts = c('crs', 'vrs', 'drs', 'irs'),
    orientation = c('in', 'out'),
    super = FALSE,
    slack = FALSE,
    peers = FALSE) {

  rts <- match.arg(rts)
  orientation <- match.arg(orientation)

  # Create model object
  model <- new_pioneer_model(data, id, input, output, rts, orientation, model_type = 'dea')

  # Calculate DEA metrics
  compute_dea_(model, super, slack, peers)

}

#' Constructor function for S3 class pioneer_model
#' @noRd
new_pioneer_model <- function(
    data, id, input, output, rts, orientation, time,
    model_type = c('dea', 'malmquist')) {

  stopifnot(is.data.frame(data))
  model_type <- match.arg(model_type)
  model <- cbind(data[input], data[output])
  attr(model, 'model') <- model_type
  attr(model, 'input') <- input
  attr(model, 'output') <- output
  if (!is.null(id)) attr(model, 'dmu') <- data[[id]]
  if (model_type == 'malmquist') {
    attr(model, 'time') <- data[[time]]
  }
  if (model_type != 'malmquist') {
    attr(model, 'rts') <- rts
  }
  attr(model, 'orientation') <- orientation
  structure(model, class = c('pioneer_model', 'data.frame'))

}

#' compute_dea_ (internal helper)
#' @inheritParams compute_dea
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
  if (slack) slack_res <- compute_slack(x, y, eff_res$unadj_values, rts = attr(model, 'rts'), orientation = attr(model, 'orientation'))
  if (peers) peers_res <- get_peers(eff_res$lambda, attr(model, 'dmu'))[-1]

  res <- list(
    efficiency = as.vector(eff_res$values)
  )
  if (super) res$super_efficiency <- as.vector(super_res$values)
  if (slack) res$slack <- as.vector(slack_res$values)
  if (peers) res$peers <- peers_res
  res$model <- model

  new_pioneer_dea(res)

}

#' Constructor function for S3 object pioneer_dea
#' @noRd
new_pioneer_dea <- function(object) {
  stopifnot(is.list(object), !is.null(object$efficiency))
  structure(object, class = 'pioneer_dea')
}
