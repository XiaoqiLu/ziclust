#' Constructor of S3 class `mixture_mdl`, it is a subclass of `mdl`
#'
#' @inheritParams new_mdl
#'
#' @return object of class `mixture_mdl`
#' @export
new_mixture_mdl <- function(params, hyper_params) {
  new_mdl(
    params,
    hyper_params,
    class = "mixture_mdl"
  )
}

#' Validator of S3 class `mixture_mdl`, it is a subclass of `mdl`
#'
#' @param model object of class `mixture_mdl`
#'
#' @return itself
#' @export
validate_mixture_mdl <- function(model) {
  params <- unclass(model)
  hyper_params <- attr(model, "hyper_params")

  stopifnot(is.list(params))
  if (is.null(params$tau) || (!is.numeric(params$tau)) || (!is.vector(params$tau))) {
    stop("should have `tau` (mixture weights, numeric vector) as parameter")
  }
  if (any(params$tau < 0) || (sum(params$tau) != 1)) {
    stop("`tau` (mixture weight) should represent a legit probability mass distribution")
  }
  if (is.null(params$components) || (!is.list(params$components))) {
    stop("should have `components` (a list) as parameter")
  }
  if (length(params$components) != length(params$tau)) {
    stop("`components` should have the same length as `tau`")
  }
  if (!all(sapply(params$components, inherits, what = "mdl"))) {
    stop("each element of `components` should be a `mdl` object")
  }

  stopifnot(is.list(hyper_params))

  model
}

#' Create an object of S3 class `mixture_mdl`
#'
#' @param tau a vector giving the weights of the mixture model
#' @param components a list of mixture components, each is a `mdl` object
#' @param hyper_params hyperparameters (not implemented)
#'
#' @return object of class `mixture_mdl`
#' @export
#'
#' @examples
#' gm <- gaussian_mdl(c(1, 2), matrix(c(1, 0, 0, 2), 2, 2))
#' mixture_mdl(c(0.2, 0.3, 0.5), rep(list(gm), 3))
mixture_mdl <- function(tau, components, hyper_params = list()) {
  params <- list(
    tau = as.vector(tau),
    components = as.list(components)
  )
  validate_mixture_mdl(new_mixture_mdl(params, hyper_params))
}

#' clustering <- function(X, g, model,
#'                        ...,
#'                        max_em_iter = 1000L,
#'                        z_tol = 1e-4) {
#'   # argument validation
#'   X <- as.matrix(X)
#'   g <- as.integer(g) # multiple g?
#'   stopifnot(g > 0)
#'   mm <-
#'   max_iter <- as.integer(max_em_iter)
#'   z_tol <- as.double(z_tol)
#'
#'   n <- nrow(X)
#'   m <- ncol(X)
#'
#'   # initialization
#'   Z <- init_belongings(n, g)
#'   mm <- mixture_model(g, component, ...)
#'
#'   # em iterations
#'   n_em_iter <- 0
#'   while (n_em_iter < max_em_iter) {
#'     n_em_iter <- n_em_iter + 1
#'     # m-step
#'     mm$tau <- colMeans(Z)
#'     for (k in 1 : g) {
#'       mm$components[[k]] <- fit(mm$components[[k]], X, weights = Z[, k])
#'     }
#'     # e-step
#'
#'   }
#' }
#'
#' #' Initialize belongings
#' #'
#' #' @param n sample size.
#' #' @param g number of clusters.
#' #'
#' #' @return a n-by-g belonging matrix.
#' #' @export
#' #'
#' #' @examples
#' #' init_belongings(100, 3)
#' init_belongings <- function(n, g) {
#'   rdirichlet(n, rep(1, g))
#' }
