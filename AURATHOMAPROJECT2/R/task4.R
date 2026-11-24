#' Model Confidence Intervals and Diagnostic Plots for Simple Linear Regression
#'
#' Takes a fitted \code{lm} object, calculates confidence intervals for the regression
#' parameters (\eqn{\beta_0}, \eqn{\beta_1}, and \eqn{\sigma^2}), and generates a
#' two-panel diagnostic plot consisting of a residual histogram with a normal
#' curve and a residuals-versus-fitted plot with a lowess smoother.
#'
#' @param x An object of class \code{lm} (simple linear regression model).
#' @param alpha Significance level for the confidence intervals (default is \code{0.05}
#'   for 95\% CIs).
#' @param ... Additional arguments passed to \code{stats::lowess()} (such as the
#'   smoothing parameter \code{f}).
#'
#' @return A named list containing:
#' \describe{
#'   \item{ci_beta0}{Confidence interval for the intercept (\eqn{\beta_0}).}
#'   \item{ci_beta1}{Confidence interval for the slope (\eqn{\beta_1}).}
#'   \item{ci_sigma2}{Confidence interval for the error variance (\eqn{\sigma^2}).}
#'   \item{point_estimates}{Named vector of point estimates for \eqn{\beta_0},
#'     \eqn{\beta_1}, and \eqn{\sigma^2}.}
#'   \item{fitted_values}{Vector of fitted values from the model.}
#'   \item{residuals}{Vector of residuals from the model.}
#' }
#'
#' @export
#' @importFrom graphics abline curve hist lines par plot
#' @importFrom stats confint dnorm qchisq sd

myciplot <- function(x, alpha = 0.05, ...) {
  # 1. Point estimates
  coefs <- x[["coefficients"]]
  b0 <- coefs[1]
  b1 <- coefs[2]

  res <- x[["residuals"]]
  yhat <- x[["fitted.values"]]
  df <- x[["df.residual"]]

  sse <- sum(res^2)
  sigma2_hat <- sse / df

  # 2. Confidence intervals (1 - alpha)
  ci_betas <- confint(x, level = 1 - alpha)
  ci_b0 <- ci_betas[1, ]
  ci_b1 <- ci_betas[2, ]

  chi_lower <- qchisq(alpha / 2, df = df)
  chi_upper <- qchisq(1 - alpha / 2, df = df)
  ci_sigma2 <- c(lower = sse / chi_upper, upper = sse / chi_lower)

  # 3. Side-by-side base R plots
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(mfrow = c(1, 2))

  # Plot a: Histogram of residuals with normal density curve
  hist(res,
       freq = FALSE,
       col = "lightblue",
       main = "Histogram of Residuals",
       xlab = "Residuals")
  curve(dnorm(x, mean = mean(res), sd = sd(res)),
        add = TRUE,
        col = "darkblue",
        lwd = 2)

  # Plot b: Residuals vs Fitted with lowess curve
  plot(yhat, res,
       pch = 19,
       col = "blue",
       main = "Residuals vs Fitted",
       xlab = "Fitted Values",
       ylab = "Residuals")
  abline(h = 0, lty = 2, col = "gray40")
  lines(stats::lowess(yhat, res, ...), col = "red", lwd = 2)

  # 4. Return named list
  out <- list(
    ci_beta0 = ci_b0,
    ci_beta1 = ci_b1,
    ci_sigma2 = ci_sigma2,
    point_estimates = c(beta0 = as.numeric(b0),
                        beta1 = as.numeric(b1),
                        sigma2 = sigma2_hat),
    fitted_values = yhat,
    residuals = res
  )

  return(out)
}
