#' Constructor of S3 class `mdl`
#'
#' @param params parameters of the model
#' @param hyper_params hyper parameters (e.g. learning rate, regularization)
#' @param ... allowing subclass
#' @param class allowing subclass
#'
#' @return newly created object
#' @export
#'
#' @examples
new_mdl <- function(params, hyper_params, ..., class = character()) {
  structure(
    params,
    hyper_params = hyper_params,
    ...,
    class = c(class, "mdl")
  )
}

#' S3 generic `generate_data`
#'
#' @param n sample size
#' @param model object of class `mdl`
#' @param ...
#'
#' @return generated data, usually a matrix with `n` rows
#' @export
#'
#' @examples
generate_data <- function(n, model, ...) {
  stopifnot(inherits(model, "mdl"))
  UseMethod("generate_data")
}

# log_prob <- function(model, X, ...) {
#   stopifnot(inherits(model, "mdl"))
#   X <- as.matrix(X)
#   UseMethod("log_prob")
# }
#
# log_lik <- function(model, X, weights = NULL, ...) {
#   stopifnot(inherits(model, "mdl"))
#   X <- as.matrix(X)
#   if (is.null(weights)) {
#     weights <- rep(1, nrow(X))
#   } else {
#     stopifnot((length(weights) == nrow(X)) && all(weights >= 0))
#   }
#   sum(weights * log_prob(model, X, ...))
# }
#
# dof <- function(model, ...) {
#   stopifnot(inherits(model, "mdl"))
#   UseMethod("dof")
# }
#
# aic <- function(model, X, ...) {
#   stopifnot(inherits(model, "mdl"))
#   2 * dof(model, ...) - 2 * log_lik(model, X, ...)
# }
#
# bic <- function(model, X, ...) {
#   stopifnot(inherits(model, "mdl"))
#   nrow(X) * dof(model, ...) - 2 * log_lik(model, X, ...)
# }
#
# fit <- function(model, X, ...) {
#   stopifnot(inherits(model, "mdl"))
#   X <- as.matrix(X)
#   UseMethod("fit")
# }
