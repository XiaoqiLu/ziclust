#' Hamiltonian of Generalized Ising Model
#'
#' @param u a binary ({-1, 1}-valued) vector
#' @param h a vector giving the external field
#' @param J a symmetric matrix with zero diagonals giving the interaction
#'
#' @return a number
#' @export
#'
#' @examples
hamiltonian_ising <- function(u, h, J) {
  return(-as.numeric(t(h) %*% u + t(u) %*% J %*% u / 2))
}
