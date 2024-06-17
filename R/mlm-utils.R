#' create_mlm_matrix
#' @noRd
create_mlm_matrix <- function(dl, value){
  lapply(dl, \(z) as.matrix(z[value]))
}

#' get_mlm_values
#' @noRd
get_mlm_values <- function(res, n_units){
  malmquist <- c(rep(NA_real_, n_units), vapply(res, \(x) x$malmquist, numeric(n_units))) |> as.vector()
  effch <- c(rep(NA_real_, n_units), vapply(res, \(x) x$effch, numeric(n_units))) |> as.vector()
  tech <- c(rep(NA_real_, n_units), vapply(res, \(x) x$tech, numeric(n_units))) |> as.vector()
  obtech <- c(rep(NA_real_, n_units), vapply(res, \(x) x$obtech, numeric(n_units))) |> as.vector()
  ibtech <- c(rep(NA_real_, n_units), vapply(res, \(x) x$ibtech, numeric(n_units))) |> as.vector()
  matech <- c(rep(NA_real_, n_units), vapply(res, \(x) x$matech, numeric(n_units))) |> as.vector()
  scale_effch <- c(rep(NA_real_, n_units), vapply(res, \(x) x$scale_effch, numeric(n_units))) |> as.vector()
  pure_effch <- c(rep(NA_real_, n_units), vapply(res, \(x) x$pure_effch, numeric(n_units))) |> as.vector()

  list(malmquist = malmquist,
       tech = tech,
       effch = effch,
       obtech = obtech,
       ibtech = ibtech,
       matech = matech,
       scale_effch = scale_effch,
       pure_effch = pure_effch)
}

#' @export
print.pioneer_mlm <- function(x, ...) {
  cat("Malmquist scores:\n")
  print(x$malmquist)
  invisible(x)
}

#' @export
summary.pioneer_mlm <- function(object, ...) {
  cat(sprintf(
    'Technology is %s and %s oriented efficiency\n',
    toupper(attr(object$model, 'rts')),
    switch(attr(object$model, 'orientation'), 'in' = 'input', 'out' = 'output')
  ))
  cat(sprintf('Mean malmquist: %s\n', round(mean(object$malmquist, na.rm = TRUE), 4L)))
  cat('-----------\n')
  summary(object$malmquist)
}

#' @export
as.data.frame.pioneer_mlm <- function(x, ...) {
  out <- list()
  out$dmu <- attr(x$model, 'dmu')
  out$time <- attr(x$model, 'time')
  out$malmquist <- x$malmquist
  out$effch <- x$effch
  out$tech <- x$tech
  out$obtech <- x$obtech
  out$ibtech <- x$ibtech
  out$matech <- x$matech
  out$scale_effch <- x$scale_effch
  out$pure_effch <- x$pure_effch
  out <- c(out, x$model)
  out <- structure(out, row.names = seq_len(dim(x$model)[1L]), class = 'data.frame')
  out <- out[order(out$dmu),] # order by DMU
  out
}
