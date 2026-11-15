#' Power Calculation for Normal Mean Testing
#'
#' Calculates the statistical power (\eqn{1 - \beta}) of a one-sample z-test
#' and generates a plot of power versus sample size \eqn{n}.
#'
#' @param n Numeric vector or integer. The sample size(s) for evaluation (or maximum \eqn{n} if plotting).
#' @param delta Numeric. The true difference or effect size (\eqn{\mu_1 - \mu_0}).
#' @param sigma Numeric. The population standard deviation.
#' @param alpha Numeric. The significance level of the test (default is 0.05).
#'
#' @return A list containing:
#' \item{power}{The calculated power value(s).}
#' \item{beta}{The type II error probability (\eqn{\beta}).}
#' \item{n}{The sample size vector used.}
#' \item{delta}{The effect size.}
#' \item{sigma}{The population standard deviation.}
#' \item{alpha}{The significance level.}
#'
#' @importFrom stats pnorm qnorm
#' @importFrom graphics plot lines title
#' @export
mypower <- function(n = 10, delta = 2, sigma = 2, alpha = 0.05) {
  # If a single value or vector is provided for n, let's support both
  # (the prompt specifies for the plot, n takes values from 0 to 30)
  n_vals <- 0:30

  # Calculate power for n from 0 to 30 for the plot
  # To avoid division by zero or negative n at n = 0, handle safely:
  power_vals <- sapply(n_vals, function(ni) {
    if (ni == 0) return(0)
    pnorm(qnorm(1 - alpha/2) - (delta * sqrt(ni)) / sigma) +
      pnorm(-qnorm(1 - alpha/2) - (delta * sqrt(ni)) / sigma)
  })

  # Plot power versus n (0 to 30)
  plot(n_vals, power_vals, type = "l", col = "black", lwd = 2,
       ylim = c(0, 1), xlim = c(0, 30),
       xlab = "n", ylab = "Power",
       main = paste0("Power Calculation: delta = ", delta, ", sigma = ", sigma, ", alpha = ", alpha))

  # Calculate power specifically for the user-supplied n input
  if (length(n) == 1) {
    target_n <- n
  } else {
    target_n <- max(n)
  }

  if (target_n == 0) {
    p_single <- 0
  } else {
    p_single <- pnorm(qnorm(1 - alpha/2) - (delta * sqrt(target_n)) / sigma) +
      pnorm(-qnorm(1 - alpha/2) - (delta * sqrt(target_n)) / sigma)
  }

  beta_val <- 1 - p_single

  # Return structured list output as shown in instructions
  list(
    power = p_single,
    beta = beta_val,
    n = target_n,
    delta = delta,
    sigma = sigma,
    alpha = alpha
  )
}
