#' Compute Malmquist
#'
#' Calculate the Malmquist productivity index and its components using Data
#' Envelopment Analysis.
#'
#' @details
#' Results are returned _a la_ Farrell. This implies that for output-oriented models
#' values above one signify improvements in productivity, while values less than
#' one imply deterioration in productivity. For input-oriented models the
#' interpretation is reversed; values less than one denote improvements
#' and values above one denote deterioration.
#'
#' Note that `compute_malmquist()` only works for balanced panel datasets.
#'
#' @inheritParams compute_dea
#' @param id A string with the DMU id or name variable.
#' @param time A string with the time period variable.
#' @return A list of class `pioneer_mlm`
#' @examples
#' # Load example data
#' chnEconomy <- deaR::EconomyLong
#' # Estimate Malmquist
#' mod <- compute_malmquist(
#'   data = chnEconomy,
#'   id = 'DMUs',
#'   time = 'Period',
#'   input = c('Labor', 'Capital'),
#'   output = 'GIOV',
#'   orientation = 'in')
#' # Print results
#' print(mod)
#' # Get summary
#' summary(mod)
#' # Convert to data frame
#' df <- as.data.frame(mod)
#' @references
#' Färe, R., Grosskopf, S. (1996). _Intertemporal production frontiers: With dynamic DEA_. Springer.
#' @export
compute_malmquist <- function(
    data,
    input,
    output,
    id,
    time,
    orientation = c('in', 'out')) {

  orientation <- match.arg(orientation)
  model <- new_pioneer_model(
    data = data,
    id = id,
    input = input,
    output = output,
    orientation = orientation,
    time = time,
    model_type = 'malmquist')
  res <- compute_malmquist_model(model)
  res
}

#' compute_malmquist_model
#' @inheritParams compute_malmquist
#' @noRd
compute_malmquist_model <- function(model) {

  # Prepare data
  model <- model[order(attr(model, 'time')),] # order by time
  ids <- attr(model, 'dmu')
  period <- attr(model, 'time')
  dl <- split(model, period)

  # Compute Malmquist
  res <- compute_mlm_pairwise(dl, model)

  structure(
    list(
      malmquist = res$malmquist,
      effch = res$effch,
      tech = res$tech,
      obtech = res$obtech,
      ibtech = res$ibtech,
      matech = res$matech,
      scale_effch = res$scale_effch,
      pure_effch = res$pure_effch,
      model = model
    ), class = 'pioneer_mlm')
}

