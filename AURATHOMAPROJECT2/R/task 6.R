#' Calculate R-squared for a Single-Knot Piecewise Regression Model
#'
#' @description
#' Fits a piecewise linear regression with a single knot (change point) at `x_k`
#' using the formula \eqn{y = \beta_0 + \beta_1 x + \zeta (x - x_k) I(x > x_k)}
#' and returns the resulting coefficient of determination (\eqn{R^2}).
#'
#' @param x_k Numeric scalar specifying the x-coordinate of the knot (change point).
#' @param x Numeric vector of predictor values.
#' @param y Numeric vector of response values.
#'
#' @return A numeric scalar representing the \eqn{R^2} value of the fitted piecewise linear model.
#'
#' @importFrom stats lm
#' @export
myknot <- function(x_k, x, y) {
  # Indicator adjustment term: (x - x_k) when x > x_k, 0 otherwise
  ix <- (x - x_k) * (x > x_k)
  
  # Fit piecewise linear model
  fit <- lm(y ~ x + ix)
  
  # Extract and return R-squared
  summary(fit)$r.squared
}