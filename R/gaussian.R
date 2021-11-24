#' Constructor of S3 class `gaussian_mdl`, it is a subclass of `mdl`
#'
#' @inheritParams new_mdl
#'
#' @return object of class `gaussian_mdl`
#' @export
new_gaussian_mdl <- function(params, hyper_params) {
  new_mdl(
    params,
    hyper_params,
    class = "gaussian_mdl"
  )
}

#' Validator of S3 class `gaussian_mdl`, it is a subclass of `mdl`
#'
#' @param model object of class `gaussian_mdl`
#'
#' @return itself
#' @export
validate_gaussian_mdl <- function(model) {
  params <- unclass(model)
  hyper_params <- attr(model, "hyper_params")

  stopifnot(is.list(params))
  if (is.null(params$mu) || (!is.numeric(params$mu)) || (!is.vector(params$mu))) {
    stop("should have `mu` (mean, numeric vector) as parameter")
  }
  if (is.null(params$sigma) || (!is.numeric(params$sigma)) || (!is.matrix(params$sigma))) {
    stop("should have `sigma` (variance, numeric matrix) as parameter")
  }
  p <- length(params$mu)
  if ((nrow(params$sigma) != p) || (ncol(params$sigma) != p)) {
    stop("dimensions of `mu` and `sigma` do not match")
  }

  stopifnot(is.list(hyper_params))

  model
}

#' Create an object of S3 class `gaussian_mdl`
#'
#' @param mu a vector giving the means of the variables
#' @param sigma a positive-definite symmetric matrix specifying the covariance matrix of the variables
#' @param hyper_params hyperparameters (not implemented)
#'
#' @return object of class `gaussian_mdl`
#' @export
#'
#' @examples
#' gaussian_mdl(c(1, 2), matrix(c(1, 0, 0, 2), 2, 2))
gaussian_mdl <- function(mu, sigma, hyper_params = list()) {
  params <- list(
    mu = as.vector(mu),
    sigma = as.matrix(sigma)
  )
  validate_gaussian_mdl(new_gaussian_mdl(params, hyper_params))
}

#' @export
generate_data.gaussian_mdl <- function(model, n = 1, ...) {
  mvtnorm::rmvnorm(n, mean = model$mu, sigma = model$sigma)
}

#' @export
log_prob.gaussian_mdl <- function(model, X, ...) {
  mvtnorm::dmvnorm(X, mean = model$mu, sigma = model$sigma, log = TRUE)
}

#' @export
dof.gaussian_mdl <- function(model) {
  d <- length(model$mu)
  d + d * (d + 1) / 2
}

#' @export
fit.gaussian_mdl <- function(model, X, weights = rep(1, nrow(X)), ...) {
  assert_weights(weights, X)
  cov_wt <- stats::cov.wt(X, weights, method = "ML")
  model$mu <- cov_wt$center
  model$sigma <- cov_wt$cov
  model
}

#' @export
init_params.gaussian_mdl <- function(model, X, weights = rep(1, nrow(X)), ...) {
  assert_weights(weights, X)
  fit.gaussian_mdl(model, X, weights, ...)
}
