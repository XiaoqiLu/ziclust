#' Constructor of S3 class `mdl`
#'
#' @param params parameters of the model
#' @param hyper_params hyper parameters (e.g. learning rate, regularization)
#' @param ... allowing subclass
#' @param class allowing subclass
#'
#' @return newly created object
#' @export
new_mdl <- function(params, hyper_params, ..., class = character()) {
  structure(
    params,
    hyper_params = hyper_params,
    ...,
    class = c(class, "mdl")
  )
}

#' S3 generic `generate_data` as RNG
#'
#' @param model object of class `mdl`
#' @param n sample size
#' @param ... arguments to be passed to methods
#'
#' @return generated data, usually a matrix with `n` rows
#' @export
generate_data <- function(model, n, ...) {
  stopifnot(inherits(model, "mdl"))
  UseMethod("generate_data")
}

#' S3 generic `log_prob` to compute log probability for each data point
#'
#' @param model object of class `mdl`
#' @param X data matrix
#' @param ... arguments to be passed to methods
#'
#' @return log probability, a numeric vector
#' @export
log_prob <- function(model, X, ...) {
  stopifnot(inherits(model, "mdl"))
  X <- as.matrix(X)
  UseMethod("log_prob")
}

#' Assert weights given data set
#'
#' @param weights weights of observations
#' @param X data matrix
#'
#' @return weights
#' @export
#'
#' @examples
#' w <- c(1, 2, 3)
#' X <- matrix(1 : 6, 3, 2)
#' assert_weights(w, X)
#'
#' \dontrun{
#' w <- c(-1, 2, 3)
#' X <- matrix(1 : 6, 3, 2)
#' assert_weights(w, X)
#' }
assert_weights <- function(weights, X) {
  stopifnot((length(weights) == nrow(X)) && all(weights >= 0))
  invisible(weights)
}

#' Compute Log-likelihood
#'
#' @inheritParams log_prob
#' @param weights weights of observations
#' @param ... arguments to be passed to `log_prob`
#'
#' @return log-likelihood, a numeric value
#' @export
log_lik <- function(model, X, weights = rep(1, nrow(X)), ...) {
  stopifnot(inherits(model, "mdl"))
  X <- as.matrix(X)
  assert_weights(weights, X)
  sum(weights * log_prob(model, X, ...))
}

#' S3 generic `dof` to compute degree of freedom
#'
#' @param model object of class `mdl`
#'
#' @return degree of freedom, a numeric value
#' @export
dof <- function(model) {
  stopifnot(inherits(model, "mdl"))
  UseMethod("dof")
}

#' Compute AIC
#'
#' @inheritParams log_lik
#' @param ... arguments to be passed to `log_lik`
#'
#' @return AIC, a numeric value
#' @export
aic <- function(model, X, weights = rep(1, nrow(X)), ...) {
  stopifnot(inherits(model, "mdl"))
  2 * dof(model) - 2 * log_lik(model, X, weights, ...)
}

#' Compute BIC
#'
#' @inheritParams log_lik
#' @param ... arguments to be passed to `log_lik`
#'
#' @return BIC, a numeric value
#' @export
bic <- function(model, X, weights = rep(1, nrow(X)), ...) {
  stopifnot(inherits(model, "mdl"))
  nrow(X) * dof(model) - 2 * log_lik(model, X, weights, ...)
}

#' S3 generic `fit` to fit model parameter given data set
#'
#' @inheritParams log_lik
#' @param ... arguments to be passed to methods
#'
#' @return fitted model
#' @export
fit <- function(model, X, weights, ...) {
  stopifnot(inherits(model, "mdl"))
  X <- as.matrix(X)
  UseMethod("fit")
}

#' S3 generic `init_params` to initialize model parameters given data set
#'
#' @inheritParams log_prob
#' @param ... arguments to be passed to methods
#'
#' @return model with parameters initialized
#' @export
init_params <- function(model, X, ...) {
  stopifnot(inherits(model, "mdl"))
  X <- as.matrix(X)
  UseMethod("init_params")
}
