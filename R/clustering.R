clustering <- function(X, g,
                       component = c("Gaussian", "Ising", "Ising-Gaussian"),
                       ...,
                       max_em_iter = 1000L,
                       z_tol = 1e-4) {
  # argument validation
  X <- as.matrix(X)
  g <- as.integer(g) # multiple g?
  stopifnot(g > 0)
  component <- march.arg(component)
  max_iter <- as.integer(max_em_iter)
  z_tol <- as.double(z_tol)

  n <- nrow(X)
  m <- ncol(X)

  # initialization
  Z <- init_belongings(n, g)
  mm <- mixture_model(g, component, ...)

  # em iterations
  n_em_iter <- 0
  while (n_em_iter < max_em_iter) {
    n_em_iter <- n_em_iter + 1
    # m-step
    mm$tau <- colMeans(Z)
    for (k in 1 : g) {
      mm$components[[k]] <- fit(mm$components[[k]], X, weights = Z[, k])
    }
    # e-step

  }
}

#' Initialize belongings
#'
#' @param n sample size.
#' @param g number of clusters.
#'
#' @return a n-by-g belonging matrix.
#' @export
#'
#' @examples
#' init_belongings(100, 3)
init_belongings <- function(n, g) {
  rdirichlet(n, rep(1, g))
}
