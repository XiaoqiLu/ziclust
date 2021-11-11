#' Standard Logistic Function
#'
#' @description
#' Equivalent to [stats::plogis()] with default arguments.
#'
#' @param x a numeric input
#'
#' @details
#' The standard logistic function is defined as
#' \deqn{p(x) = \frac{1}{1 + e^{-x}}.}
#' We decided to write our own implementation instead of using the C/C++ powered
#' [stats::plogis()] after benchmarking performance.
#'
#' @return a (0, 1)-ranged output of the same size as `x`
#' @export
#'
#' @examples
#' logistic(0)
logistic <- function(x) {
  1 / (1 + exp(-x))
}

#' RowWise Kronecker
#'
#' @description
#' `rowwise_kronecker` is a rowwise Kronecker utility function. It is designed
#' for feature engineering, but we use it to create quadratic/interaction terms
#' of config.
#'
#' @param x the first data matrix
#' @param y the second data matrix, should have the same number of rows as `x`
#'
#' @return transformed matrix
#' @export
#'
#' @examples
#' x <- matrix(
#'   c(
#'     1, 1,
#'     1, 2,
#'     1, 3
#'   ),
#'   3, 2,
#'   byrow = TRUE
#' )
#' y <- matrix(
#'   c(
#'     1, 0,
#'     0, 1,
#'     1, 1
#'   ),
#'   3, 2,
#'   byrow = TRUE
#' )
#' rowwise_kronecker(x, y)
rowwise_kronecker <- function(x, y) {
  nx <- ncol(x)
  ny <- ncol(y)
  x[, rep(seq(nx), times = ny), drop = FALSE] * y[, rep(seq(ny), each = nx), drop = FALSE]
}

#' RNG for Dirichlet Distribution
#'
#' @param n number of observations.
#' @param alphas vector of concentration parameters.
#'
#' @return a matrix where each row is one observation.
#' @export
#'
#' @examples
#' Z <- rdirichlet(10, c(2, 4, 8))
rdirichlet <- function(n, alphas) {
  g <- length(alphas)
  Z <- matrix(stats::rgamma(n * g, shape = alphas, rate = 1), n, g, byrow = TRUE)
  Z / rowSums(Z)
}
