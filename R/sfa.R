#' @importFrom stats lm model.frame model.response optim pnorm dnorm
#' @title Stochastic Frontier Analysis
#' @description This function computes a stochastic frontier model using the half normal model for the efficiencies.
#' The fitting method is Maximum Likelihood and both Production and Cost Functions can be estimated.
#'
#'
#' @param formula a two-sided linear formula object describing both the fixed-effects part of the model, with the response on the left of a ~ operator and the terms, separated by + operators, on the right.
#' @param data a data frame containing the variables named in formula. By default the variables are taken from the environment from which sfa is called.
#' @param pars Starting values for the algorithm.
#' @param productivity Boolean indicating productivity (TRUE) or cost analysis (FALSE).
#' @return \item{coef}{a vector of coefficients of the model. The coefficients start with the intercept, followed by the regression coefficients of the variables. The vector ends with the error variance, and the variance of the half normal distribution}
#' @return \item{hess}{the hessian of the fitted model}
#' @return \item{loglik}{the optimized log likelihood}
#' @return \item{ols}{regression coefficients for the starting values}
#' @return \item{sigmau2}{variance of the half normal distribution}
#' @return \item{sigmav2}{variance of the errors}
#' @return \item{sigma2}{The estimate of the total variance, that is, the sum of two sigma's}
#' @return \item{gamma}{Ratio of the variance of the half normal distribution over the total variance}
#' @return \item{convergence}{If the value is zero the model converged, other values indicate problems during iterations}
#' @return \item{ptable}{summary table with coefficients, standard errors, z values, and p values}
#'
#' @export
#'
#'
sfa <- function(formula, data = NULL,   pars = NULL,
                productivity = TRUE)
{
  mf <- model.frame(formula, data)
  y <- model.response(mf)
  x <- mf[-1]
  nobs <- length(y)
  # Error messages
  if(!is.vector(y)) stop("y is not a vector")
  if(!is.data.frame(x)) stop("x is not a data frame")
  if(nobs != nrow(x)) stop("x and y lengths differ")

  ols <- lm(y ~ ., data = x)
  options(warn=-1)
  nvar <- ncol(x) + 1
  X <- as.matrix(x)

  # type of form
  if (!productivity) sc = -1
  if (productivity) sc = 1
  # intercept
  X <- cbind(1, X)

  #########starting values############
  if (is.null(pars)) {
    mean_resid <- mean(ols$residuals^2) #m2
    if(sc*sum(ols$residuals^3)<0) {
      m3 <- mean(ols$residuals^3)
    } else
    {
      m3 <- -0.0001*sc
    }
    su <- var(ols$residuals)*(1-2/pi)
    su2 <- (sc*m3/(sqrt(2/pi)*(1-4/pi)))^(1/3)
    sv2 <- mean_resid - (1-2/pi)*su2
    if(sv2 <= 0)
      sv2 <- 0.00001
    sv <- sqrt(sv2)
    ### b0
    b0 <- as.matrix(coef(ols)[1]+sc*(sqrt(2/pi))*su)
    pars <- c(su2, sv2, b0, ols$coefficients[2:nvar])
    names(pars) <- c("sigmau2", "sigmav2", "Intercept", names(pars)[4:length(pars)])
  }
  log_loss <- function(p) {
    b <- p[3:length(p)]
    sigmau2_dmu <- p[1]
    sigmav2 <- p[2]
    residuals <- (y - X%*%b)
    sigma2 <- sigmau2_dmu + sigmav2
    value <- -sc * residuals * sqrt(sigmau2_dmu / (sigmav2 * sigma2))
    ret = (nobs / 2 * log(2 / pi) - nobs / 2 * log(sigma2)
           + sum(log(pnorm(value))) - 1 / (2 * sigma2) * sum(residuals^2))
    names(ret) <- "Log-Lik normal/half-normal distribution"
    return(-ret)
  }
  log_loss_gradient <- function(p) {
    b <- p[3:length(p)]
    sigmau2_dmu <- p[1]
    sigmav2 <- p[2]
    residuals <- (y - X%*%b)

    sigma2 <- sigmau2_dmu + sigmav2
    value <- -sc * residuals * sqrt(sigmau2_dmu / (sigmav2 * sigma2))
    sc_dnorm_over_pnorm = sc * dnorm(value) / pnorm(value)

    # calculate partial derivatives
    dlog_loss_dsiggmau2_dmu <- (-nobs / (2 * sigma2)
                                - sum(sc_dnorm_over_pnorm * residuals / (2 * sqrt(sigmav2)) *
                                        (1 / sqrt(sigmau2_dmu * sigma2) - sqrt(sigmau2_dmu) / sigma2^(3/2)))
                                + 1 / (2 * sigma2^2) * sum(residuals^2))

    dlog_loss_dsigmav2      <- (-nobs / (2 * sigma2)
                                + sum(sc_dnorm_over_pnorm * residuals * sqrt(sigmau2_dmu) / 2 *
                                        (1 / (sigma2^(3/2) * sqrt(sigmav2)) + 1 / ( sqrt(sigma2) * sigmav2^(3/2))))
                                + 1 / (2 * sigma2^2) * sum(residuals^2))

    dlog_loss_db <- (
      colSums(X * as.vector(sc_dnorm_over_pnorm * sqrt(sigmau2_dmu / (sigmav2 * sigma2))))
      + as.numeric(t(X)%*%residuals / sigma2))

    gradient_ret <- c(dlog_loss_dsiggmau2_dmu, dlog_loss_dsigmav2, dlog_loss_db)
    return(-gradient_ret)
  }

  q <- optim(par = pars, fn = log_loss, gr = log_loss_gradient,
             method = "L-BFGS-B",
             lower = c(.01, .01, rep(-Inf, nvar)), hessian = TRUE )
  coef <- q$par
  sigma2 <- coef["sigmau2"] + coef["sigmav2"]
  names(sigma2) <- "sigma2"
  gamma <- coef["sigmau2"]/sigma2
  names(gamma) <- "gamma"
  LogLik <- q$value
  sd <- sqrt(diag(solve(q$hess)))
  z <- coef/sd
  p.value <- 2 * (1 - pnorm(abs(z)))
  ptable <- data.frame(coefficient = coef, SE = sd, z = z, p.value = p.value)
  rownames(ptable) <- names(coef)
  result <- list(coef = coef, hess = q$hessian, logLik = LogLik,
                 ols = ols, sigmau2 = coef["sigmau2"], sigmav2 = coef["sigmav2"],
                 sigma2 = sigma2, gamma = gamma, ptable = ptable, convergence = q$convergence)
  class(result) <- "p_ssf"
  return(result)
}
