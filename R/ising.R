#' Hamiltonian of Generalized Ising Model
#'
#' @param u a binary ({-1, 1}-valued) vector or matrix. If `u` is a matrix, each
#' row is taken as an input vector.
#' @param h a vector giving the external field.
#' @param J a symmetric matrix with zero diagonals giving the interaction.
#'
#' @return a number
#' @export
#'
#' @examples
hamiltonian_ising <- function(u, h, J) {
  if (is.vector(u)) {
    return(-as.numeric(t(h) %*% u + t(u) %*% J %*% u / 2))
  } else if (is.matrix(u)) {
    return(apply(u, 1, hamiltonian_ising, h = h, J = J))
  } else {
    stop("type error: u should be a vector or matrix")
  }
}
