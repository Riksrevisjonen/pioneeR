#' Create linear program
#' @inheritParams compute_efficiency
#' @param m Number of inputs.
#' @param n Number of outputs.
#' @param n_units Number of units (DMUs).
#' @param n_constraints Number of constraints.
#' @param n_vars Number of decision variables.
#' @param n_lambda Number of lambda variables.
#' @param slack If `TRUE` creates a model for slack analysis.
#' @noRd
create_lp <- function(x, y,
                      xref = NULL,
                      yref = NULL,
                      type,
                      orientation,
                      m, n,
                      n_units,
                      n_constraints,
                      n_vars,
                      n_lambda = NULL,
                      slack = FALSE) {
  if (is.null(xref) && is.null(yref)) {
    lp <- create_lp_(
      x, y,
      type = type,
      orientation = orientation,
      m = m,
      n = n,
      n_units = n_units,
      n_constraints = n_constraints,
      n_vars = n_vars,
      n_lambda = n_lambda,
      slack = slack)
  } else {
    lp <- create_lp_(
      xref, yref,
      type = type,
      orientation = orientation,
      m = m,
      n = n,
      n_units = n_units,
      n_constraints = n_constraints,
      n_vars = n_vars,
      n_lambda = n_lambda,
      slack = slack)
  }
  return(lp)
}


#' Create linear program
#' @inheritParams create_lp
#' @noRd
create_lp_ <- function(x, y, type, orientation, m, n, n_units, n_constraints,
                       n_vars, n_lambda, slack) {

  lp <- lpSolveAPI::make.lp(n_constraints, n_vars)
  if (slack) {
    lpSolveAPI::set.objfn(lp, rep(1, m + n), 1:(m + n))
    lpSolveAPI::set.constr.type(lp, c(rep('=', m + n), rep('<=', n_lambda)))
    lpSolveAPI::lp.control(lp, sense='max')
  } else {
    lpSolveAPI::set.objfn(lp, 1, 1)
    lpSolveAPI::set.constr.type(lp, rep('>=', n_constraints))
    if (orientation == 'in') lpSolveAPI::lp.control(lp, sense = 'min')
    if (orientation == 'out') lpSolveAPI::lp.control(lp, sense = 'max')
  }
  lpSolveAPI::lp.control(lp, scaling = c('range', 'equilibrate', 'integers'))
  lp <- set_lp_constraints(lp, x, y, type, m = m, n = n, n_units = n_units,
                           n_vars = n_vars, slack = slack)
  return(lp)
}

#' set_lp_constraints
#' @param lp An lpSolve linear program model object.
#' @inheritParams create_lp
#' @noRd
set_lp_constraints <- function(lp, x, y, type, m, n, n_units, n_vars, slack) {
  if (slack) {
    l1 <- m+n+1
    # Set restrictions on matrix
    for (h in seq_len(m)) lpSolveAPI::set.row(lp, h, x[,h], l1:n_vars) # (m+n+1):(m+n+n_units)
    for (h in seq_len(n)) lpSolveAPI::set.row(lp, m+h, -y[,h], l1:n_vars) # (m+n+1):(m+n+n_units)
    for (h in seq_len(m+n)) lpSolveAPI::set.mat(lp, h, h, 1)
  } else {
    # Set restrictions on input/output
    for (h in seq_len(m)) lpSolveAPI::set.row(lp, h, c(0, -x[,h])) # inputs
    for (h in seq_len(n)) lpSolveAPI::set.row(lp, m+h, c(0, y[,h])) # outputs
  }
  lp <- set_lambda_constraints(lp, type = type, m = m, n = n,
                               n_units = n_units, slack = slack)
  return(lp)
}

#' set_lambda_constraints
#' @noRd
set_lambda_constraints <- function(lp, type, m, n, n_units, slack){
  if (slack) q <- rep(0, m+n) else q <- 0 # Set number of starting zeros
  if (type != 'crs') {
    l1 <- m+n+1; l2 <- m+n+2
    if (type == 'vrs') {
      # lpSolveAPI::set.row(lp, l1, c(0, rep(-1, n_units)))
      # lpSolveAPI::set.row(lp, l2, c(0, rep(1, n_units)))
      lpSolveAPI::set.row(lp, l1, c(q, rep(-1, n_units)))
      lpSolveAPI::set.row(lp, l2, c(q, rep(1, n_units)))
      lpSolveAPI::set.rhs(lp, c(-1, 1), l1:l2)
    } else if (type == 'irs') {
      # lpSolveAPI::set.row(lp, l1, c(0, rep(1, n_units)))
      lpSolveAPI::set.row(lp, l1, c(q, rep(1, n_units)))
      lpSolveAPI::set.rhs(lp, 1, l1)
      if (slack) lpSolveAPI::set.constr.type(lp, '>=', l1)
    } else if (type == 'drs') {
      # lpSolveAPI::set.row(lp, l1, c(0, rep(-1, n_units)))
      lpSolveAPI::set.row(lp, l1, c(q, rep(-1, n_units)))
      lpSolveAPI::set.rhs(lp, -1, l1)
      if (slack) lpSolveAPI::set.constr.type(lp, '>=', l1)
    }
  }
  return(lp)
}

#' solve_lp
#' @inheritParams create_lp
#' @noRd
solve_lp <- function(lp, x, y, orientation, m, n) {

  # Solve LP for each row
  dl <- mapply(\(x, y) {
    solve_lp_single(x, y, lp, orientation, m = m, n = n)
  }, x = asplit(x, 1), y = asplit(y, 1))

  values <- vapply(dl, \(x) x$value, FUN.VALUE = numeric(1))
  lambda <- do.call('rbind', lapply(dl, \(x) x$solution)) |> as.matrix()
  eps <- lpSolveAPI::lp.control(lp)$epsilon['epsint']

  list(
    values = values,
    lambda = lambda,
    eps = eps
  )
}

#' solve_lp_single
#' @noRd
solve_lp_single <- function(x, y, lp, orientation, m, n){
  if (orientation == 'in') {
    lpSolveAPI::set.column(lp, 1, c(1, x), 0:m)
    lpSolveAPI::set.rhs(lp, y, (m + 1):(m + n))
  } else {
    lpSolveAPI::set.column(lp, 1, c(1, -y), c(0, (m + 1):(m + n)))
    lpSolveAPI::set.rhs(lp, -x, 1:m)
  }
  lpSolveAPI::set.basis(lp, default = TRUE)
  status <- lpSolveAPI::solve.lpExtPtr(lp)
  value <- get_lp_objective(lp, status, orientation)
  solution <- get_lp_solution(lp, status)[-1]
  list(list(value = value, solution = solution))
}

#' solve_lp_slack
#' @inheritParams create_lp
#' @noRd
solve_lp_slack <- function(lp, x, y, values, orientation, m, n, n_units, n_vars) {

  if (orientation == 'in') {
    e_values <- values
    f_values <- rep(1, n_units)
  } else if (orientation == 'out') {
    f_values <- values
    e_values <- rep(1, n_units)
  }
  rhs <- cbind(e_values * x, -f_values * y)
  mn <- m+n
  mn1 <- mn+1
  values <- rep(NA_real_, n_units)
  sx <- matrix(NA_real_, n_units, m)
  sy <- matrix(NA_real_, n_units, n)
  lambda <- matrix(NA_real_, n_units, n_units)

  for (i in seq_len(n_units)) {
    lpSolveAPI::set.rhs(lp, rhs[i,], 1:mn)
    lpSolveAPI::set.basis(lp, default=TRUE)
    status <- solve(lp)
    if (status != 0) {
      if (status == 2 || status == 3) {
        values[i] <- 0
        solution <- rep(0, n_vars) #m+n+Kr
        solution[mn+i] <- 1
      } else {
        # message('Error in solving for unit ', i, ': Status = ', status)
        values[i] <- NA_real_
        solution <- NA_real_
      }
    } else {
      values[i] <- lpSolveAPI::get.objective(lp)
      solution <- lpSolveAPI::get.variables(lp)
    }
    sx[i,] <- solution[1:m]
    sy[i,] <- solution[(m+1):mn]
    lambda[i,] <- solution[mn1:n_vars]
  }
  colnames(sx) <- paste('sx',1:m,sep='')
  colnames(sy) <- paste('sy',1:n,sep='')
  #colnames(sx) <- sprintf('slack_x_%s', colnames(x))
  #colnames(sy) <- sprintf('slack_y_%s', colnames(y))

  out <- list(
    lp = lp,
    values = values,
    sx = sx,
    sy = sy,
    lambda = lambda
  )
}

#' scale_vars (for slack analysis)
#' @inheritParams create_lp
#' @noRd
scale_vars <- function(x, y, m, n, n_units) {
  mmm <- colMeans(x)
  nnn <- colMeans(y)
  if (min(mmm) < 1e-4 || max(mmm) > 1e4 ||
      min(nnn) < 1e-4 || max(nnn) > 1e4) {
    x <- x / matrix(mmm, nrow=n_units, ncol=m, byrow=TRUE)
    y <- y / matrix(nnn, nrow=n_units, ncol=n, byrow=TRUE)
    scale <- TRUE
  } else {
    scale = FALSE
  }
  list(x=x, y=y, mmm = mmm, nnn = nnn, scale = scale)
}

#' adjust_values
#' @noRd
adjust_values <- function(unadj_values, lambda, eps, digits = NULL) {
  # efficiency scores close to 1 are converted to 1
  values <- adjust_efficiency(unadj_values, eps)
  # lambda values close to 0 and 1 are converted to 0 and 1 respectively
  lambda <- adjust_lambda(lambda, eps)
  # round digits
  if (!is.null(digits)) {
    values <- round(values, digits)
    unadj_values <- round(unadj_values, digits)
    lambda <- round(lambda, digits)
  }
  out <- list(
    values = values,
    unadj_values = unadj_values,
    lambda = lambda)
  return(out)
}

#' adjust_slack
#' @noRd
adjust_slack <- function(lp, values, sx, sy, lambda, scaling, m, n, n_units, digits = NULL){
  # For at undgaa at regneunoejagtigheder giver ikke-nul slack bliver
  # slack sat til nul hvis de er mindre end den relative noejagitghed
  # i input og output, noejagtighed der er i beregningerne.  Det
  # betyder at sum af slacks bliver sum mens objval er sum plus
  # regneunoejagtighed som giver en forskel hvis X er meget stoerre
  # eller mindre end 1.
  lpcontr <- lpSolveAPI::lp.control(lp)
  eps <- lpcontr$epsilon['epsint']
  lambda <- adjust_lambda(lambda, eps)
  sx[abs(sx) < eps ] <- 0
  sy[abs(sy) < eps ] <- 0
  if (scaling$scale) {
    sx <- sx * matrix(scaling$mmm, nrow=n_units, ncol=m, byrow=TRUE)
    sy <- sy * matrix(scaling$nnn, nrow=n_units, ncol=n, byrow=TRUE)
  }
  sum <- rowSums(sx) + rowSums(sy)
  is_slack <- sum > eps
  # round digits
  if (!is.null(digits)) {
    sum <- round(sum, digits)
    values <- round(values, digits)
    lambda <- round(lambda, digits)
    sx <- round(sx, digits)
    sy <- round(sy, digits)
  }
  list(values = sum,
       unadj_values = values,
       lambda = lambda,
       data = data.frame(sum = sum, is_slack, sx, sy)
  )
}

#' adjust_efficiency
#' @noRd
adjust_efficiency <- function(x, eps){
  x[abs(x-1) < eps] <- 1
  return(x)
}

#' adjust_lambda
#' @noRd
adjust_lambda <- function(x, eps) {
  x[abs(x-1) < eps] <- 1   # 'lambda' close to 1 should be 1
  x[abs(x) < eps] <- 0     # 'lambda' close to 0 should be 0
  return(x)
}

#' Get model dimensions
#' @inheritParams create_lp
#' @noRd
get_dims <- function(x, y, xref = NULL, yref = NULL, type, slack = FALSE) {
  # type <- match.arg(type)
  n_inputs <- ncol(x)
  n_outputs <- ncol(y)
  #n_units <- nrow(x)
  if (!is.null(xref)) n_units <- nrow(xref) else n_units <- nrow(x)
  if (type == 'crs') {
    n_lambda <- 0
  } else if (type %in% c('irs', 'drs', 'fdh')) {
    n_lambda <- 1
  } else {
    n_lambda <- 2
  }
  if (slack) {
    n_vars <- n_inputs + n_outputs + n_units
  } else {
    n_vars <- 1 + n_units
  }
  list(
    n_inputs = n_inputs,
    n_outputs = n_outputs,
    n_units = n_units,
    n_constraints = n_inputs + n_outputs + n_lambda,
    n_vars = n_vars,
    n_lambda = n_lambda
  )
}


#' get_invalid_objective
#' @noRd
get_invalid_objective <- function(status, orientation){
  if (status != 0) {
    if (status == 2 || status == 3) {
      # if the unit is not in the technology set the value to Inf
      if (orientation == 'in') value <- Inf else value <- -Inf
    } else {
      value <- NA_real_
    }
  }
  return(value)
}

#' get_lp_objective
#' @noRd
get_lp_objective <- function(lp, status, orientation) {
  if (status != 0) {
    get_invalid_objective(status, orientation)
  } else {
    lpSolveAPI::get.objective(lp)
  }
}

#' get_lp_solution
#' @noRd
get_lp_solution <- function(lp, status) {
  if (status != 0) {
    n <- lpSolveAPI::dim.lpExtPtr(lp)[2]
    rep(NA_real_, n)
  } else {
    lpSolveAPI::get.variables(lp)
  }
}