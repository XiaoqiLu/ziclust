#' Hamiltonian of Generalized Ising Model
#'
#' @description
#' `hamiltonian_ising` is used to compute the energy of given configuration in a
#' generalized Ising model.
#'
#' @param u a binary ({-1, 1}-valued) vector or matrix. If `u` is a matrix, each
#' row is taken as an input vector.
#' @param h a vector giving the external field.
#' @param J a symmetric matrix with zero diagonals giving the interaction. If
#' `J` is `NULL`, the model degenerates to the case of independent binary.
#'
#' @details
#' The generalized Ising model (also known as quadratic exponential binary
#' distribution) is generalizing the adjacent coupling of Ising model to two-way
#' interactions. The probability mass function of generalized Ising model is
#' \deqn{P(u \vert h, J) = \frac{1}{Z(h, J)} e^{- H(u; h, J)},}
#' where \eqn{H(u; h, J) = - h^T u - \frac{1}{2} u^T J u} is the Hamiltonian.
#'
#' @return a vector giving the energy of configuration u
#' @export
#'
#' @examples
#' hamiltonian_ising(
#'   u = c(+1, +1),
#'   h = c(0, 0),
#'   J = matrix(c(0, -1, -1, 0), 2, 2)
#' )
#'
#' hamiltonian_ising(
#'   u = matrix(c(-1, +1, +1, -1), 2, 2, byrow = TRUE),
#'   h = c(2, 3),
#'   J = matrix(0, 2, 2)
#' )
hamiltonian_ising <- function(u, h, J = NULL) {
  if (is.vector(u)) u <- matrix(u, nrow = 1)

  if (is.null(J)) {
    return(as.vector(-u %*% h))
  } else {
    return(as.vector(-u %*% h - rowwise_kronecker(u, u) %*% as.vector(J) / 2))
  }
}

#' Diff in Energy of Generalized Ising Model by Single Flip
#'
#' @description
#' `diff_flipped_hamilton_ising` is a fast-computation method for energy
#' difference, used in the case where only one variable is flipped.
#'
#' @param j the (column) index of the variable to be flipped over
#' @inheritParams hamiltonian_ising
#'
#' @details
#' \deqn{\Delta_j E(u \vert h, J) = H(\tilde{u}; h, J) - H(u; h, J),}
#' where \eqn{\tilde{u}_j = - u_j} and \eqn{\tilde{u}_{-j} = u_{-j}}. It can be
#' shown that \eqn{\Delta_j E(u \vert h, J) = 2 u_j (h_j + J_{j, -j} u_{-j})}.
#'
#' @return a vector giving the energy difference
#' @export
#'
#' @examples
#' h <- c(2, 3)
#' J <- matrix(c(0, -1, -1, 0), 2, 2)
#' u <- c(-1, +1)
#' diff_flipped_hamiltonian_ising(u, 2, h, J)
#'
#' u <- matrix(c(-1, +1, +1, -1), 2, 2, byrow = TRUE)
#' diff_flipped_hamiltonian_ising(u, 1, h, J)
diff_flipped_hamiltonian_ising <- function(u, j, h, J = NULL) {
  if (is.vector(u)) u <- matrix(u, nrow = 1)

  if (is.null(J)) {
    return(as.vector(2 * u[, j, drop = FALSE] * h[j]))
  } else {
    return(
      as.vector(
        2 * u[, j, drop = FALSE] *
          (h[j] + u[, -j, drop = FALSE] %*% J[-j, j, drop = FALSE])
      )
    )
  }
}

#' Gibbs Sampler for Generalized Ising Model
#'
#' @description
#' `gibbs_ising` is a Gibbs sampler generates data from Ising model, provided
#' starting config and model parameters. It is recommended that user uses a
#' multi-chain approach, that is, providing a matrix `u`.
#'
#' @param n_step the number of Gibbs steps, default = 1
#' @inheritParams hamiltonian_ising
#'
#' @return config after Gibbs updates
#' @export
#'
#' @examples
#' n <- 1000
#' p <- 3
#' h <- rnorm(p)
#' J <- matrix(0, p, p)
#' u <- matrix(2 * (runif(n * p) < 0.5) - 1, n, p)
#' gibbs_ising(u, h, J)
gibbs_ising <- function(u, h, J, n_step = 1) {
  if (is.vector(u)) u <- matrix(u, nrow = 1)
  dim_u <- dim(u)

  for (i_step in 1:n_step) {
    for (j in 1:dim_u[2]) {
      energy_diff_flipped <- diff_flipped_hamiltonian_ising(u, j, h, J)
      p_flipped <- logistic(energy_diff_flipped)
      sign_flipped <- 2 * (stats::runif(dim_u[1]) < p_flipped) - 1
      u[, j] <- sign_flipped * u[, j]
    }
  }
  return(u)
}
