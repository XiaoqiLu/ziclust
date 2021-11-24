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
  if (is.null(hyper_params$max_iter) || (!is.numeric(hyper_params$max_iter)) || (length(hyper_params$max_iter) > 1)) {
    stop("should have `max_iter` (maximum iterations, numeric value) as hyperparameter")
  }
  if (hyper_params$max_iter < 0) {
    stop("`max_iter` should be non-negative")
  }
  if (is.null(hyper_params$z_tol) || (!is.numeric(hyper_params$z_tol)) || (length(hyper_params$z_tol) > 1)) {
    stop("should have `z_tol` (cluster belonging precision, numeric value) as hyperparameter")
  }
  if (hyper_params$z_tol <= 0) {
    stop("`z_tol` should be positive")
  }

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
#' gm1 <- gaussian_mdl(c(1, 2), matrix(c(1, 0, 0, 2), 2, 2))
#' gm2 <- gaussian_mdl(c(-5, -3), matrix(c(2, -1, -1, 1), 2, 2))
#' mixture_mdl(c(0.2, 0.8), list(gm1, gm2))
mixture_mdl <- function(tau, components, hyper_params = list()) {
  params <- list(
    tau = as.vector(tau),
    components = as.list(components)
  )

  hyper_params_names <- names(hyper_params)
  hp <- list(max_iter = 100L, z_tol = 1e-3)
  hp_names <- names(hp)
  hyper_params_names_matched <- hyper_params_names[hyper_params_names %in% hp_names]
  if (length(hyper_params_names_matched) < length(hyper_params_names)) {
    warning("`hyper_params`: unknown names beyond ", paste0(hp_names, collapse = ", "), " detected")
  }
  hp[hyper_params_names_matched] <- hyper_params[hyper_params_names_matched]

  validate_mixture_mdl(new_mixture_mdl(params, hp))
}

#' @export
generate_data.mixture_mdl <- function(model, n = 1, ...) {
  Z <- t(stats::rmultinom(n, 1, model$tau))
  X <- 0
  for (k in 1 : length(model$tau)) {
    X <- X + Z[, k] * generate_data(model$components[[k]], n, ...)
  }
  X
}

#' @export
log_prob.mixture_mdl <- function(model, X, ...) {
  logp <- sapply(model$components, log_prob, X, ...)
  apply(matrix(model$tau, nrow(X), length(model$tau), byrow = TRUE) + logp,
        1, log_sum_exp)
}

#' Compute belongings, which is useful in clustering EM algorithm (E-step)
#'
#' @inheritParams log_prob
#' @param ... arguments passed to `log_prob`
#'
#' @return the Z matrix (n-by-g)
#' @export
#'
#' @examples
#' gm1 <- gaussian_mdl(c(1, 2), matrix(c(1, 0, 0, 2), 2, 2))
#' gm2 <- gaussian_mdl(c(-5, -3), matrix(c(2, -1, -1, 1), 2, 2))
#' gm3 <- gaussian_mdl(c(0, 0), matrix(c(1, 0.3, 0.3, 1), 2, 2))
#' gmm <- mixture_mdl(c(0.3, 0.2, 0.5), list(gm1, gm2, gm3))
#' X <- generate_data(gmm, 1000)
#' Z <- compute_belongings(gmm, X)
compute_belongings <- function(model, X, ...) {
  stopifnot(inherits(model, "mixture_mdl"))
  logp <- sapply(model$components, log_prob, X, ...)
  t(apply(matrix(model$tau, nrow(X), length(model$tau), byrow = TRUE) + logp,
          1, soft_max))
}

#' @export
dof.mixture_mdl <- function(model) {
  length(model$tau) + sum(sapply(model$components, dof))
}

#' @export
init_params.mixture_mdl <- function(model, X, weights = rep(1, nrow(X)), ...) {
  assert_weights(weights, X)
  g <- length(model$tau)
  Z <- rdirichlet(nrow(X), rep(1, g))
  model$tau <- apply(Z, 2, stats::weighted.mean, w = weights)
  for (k in 1 : g) {
    model$components[[k]] <- init_params(model$components[[k]], X, weights * Z[, k], ...)
  }
  model
}

#' @export
fit.mixture_mdl <- function(model, X, weights = rep(1, nrow(X)), ...) {
  assert_weights(weights, X)
  g <- length(model$tau)
  hyper_params <- attr(model, "hyper_params")
  n_iter <- 0L
  is_converged <- FALSE
  Z_prev <- 0
  while ((n_iter < hyper_params$max_iter) && (!is_converged)) {
    n_iter <- n_iter + 1L
    # e-step: compute Z
    Z <- compute_belongings(model, X, ...)
    # m-step: compute parameters (fit each component)
    model$tau <- apply(Z, 2, stats::weighted.mean, w = weights)
    for (k in 1 : g) {
      model$components[[k]] <- fit(model$components[[k]], X, weights * Z[, k], ...)
    }
    # check convergence
    is_converged <- max(abs(Z - Z_prev)) < hyper_params$z_tol
    Z_prev <- Z
  }
  if (!is_converged) {
    print(paste("not converged in", n_iter, "iterations"))
  } else {
    print(paste("converged in", n_iter, "iterations"))
  }
  model
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
