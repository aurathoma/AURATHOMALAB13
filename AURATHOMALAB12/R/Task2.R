#' Bootstrap Distribution of Likelihood Ratio and Ybar
#'
#' Performs a Monte Carlo bootstrap simulation to discover the empirical distributions
#' of \eqn{\log(\lambda)} and \eqn{\bar{Y}}, calculates critical cutoff values for
#' hypothesis testing, and generates distribution histograms with cutoff lines.
#'
#' @param iter Numeric. The number of bootstrap simulation iterations (default is 10000).
#' @param y Numeric vector. The original sample data used for resampling.
#' @param alpha Numeric. The significance level for the critical cutoffs (default is 0.05).
#'
#' @return A list containing:
#' \item{xstat1}{Bootstrap values of \eqn{\log(\lambda)}.}
#' \item{xstat2}{Bootstrap values of \eqn{\bar{Y}}.}
#' \item{loglamcut}{The empirical cutoff value for \eqn{\log(\lambda)} at level \eqn{\alpha}.}
#' \item{ybarcut}{The empirical cutoff value for \eqn{\bar{Y}} at level \eqn{1-\alpha}.}
#' \item{sam}{An \code{iter} x \eqn{n} matrix of resampled data values.}
#' \item{mat}{An alias of the resampling matrix.}
#' \item{ybarcutest}{The theoretical cutoff value for \eqn{\bar{Y}} calculated from the formula.}
#'
#' @importFrom stats quantile
#' @importFrom graphics hist abline
#' @export
mylambda <- function(iter = 10000, y, alpha = 0.05) {
  n <- length(y)

  # Initialize vectors/matrices to store results
  xstat1 <- numeric(iter) # bootstrap values of log lambda
  xstat2 <- numeric(iter) # bootstrap values of ybar
  mat <- matrix(NA, nrow = iter, ncol = n) # matrix of resampled y values

  for (i in 1:iter) {
    # Resample with replacement
    resampled_y <- sample(y, size = n, replace = TRUE)
    mat[i, ] <- resampled_y

    # Compute statistics for this iteration
    ybar_star <- mean(resampled_y)
    xstat2[i] <- ybar_star
    xstat1[i] <- -n * (ybar_star^2) / 2 # log(lambda) = -n*ybar^2 / 2
  }

  # Find alpha cutoffs
  loglambda_cut <- quantile(xstat1, probs = alpha)
  ybar_cut <- quantile(xstat2, probs = 1 - alpha)

  # Theoretical ybar cutoff from formula: sqrt(-2 * log(lambda_alpha) / n)
  ybar_cut_formula <- sqrt(-2 * loglambda_cut / n)

  # Plot 1: Distribution of log(lambda)
  hist(xstat1, probability = TRUE, col = "sienna",
       main = "Distribution of log(lambda)", xlab = "log(lambda)")
  abline(v = loglambda_cut, col = "blue", lwd = 2)

  # Plot 2: Distribution of Ybar
  hist(xstat2, probability = TRUE, col = "khaki",
       main = "Distribution of Ybar", xlab = "Ybar")
  abline(v = ybar_cut, col = "blue", lwd = 2)

  # Return structured list output
  list(
    xstat1 = xstat1,
    xstat2 = xstat2,
    loglamcut = loglambda_cut,
    ybarcut = ybar_cut,
    sam = mat,
    mat = mat,
    ybarcutest = ybar_cut_formula
  )
}
