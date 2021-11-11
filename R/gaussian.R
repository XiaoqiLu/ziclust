#' Constructor of S3 class `gaussian_mdl`, it is a subclass of `mdl`
#'
#' @param params
#' @param hyper_params
#'
#' @return object of class `gaussian_mdl`
#' @export
#'
#' @examples
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
#'
#' @examples
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

gaussian_mdl <- function(mu, sigma, hyper_params = list()) {
  params <- list(
    mu = as.vector(mu),
    sigma = as.matrix(sigma)
  )
  validate_gaussian_mdl(new_gaussian_mdl(params, hyper_params))
}
