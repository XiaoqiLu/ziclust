% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ising.R
\name{diff_flipped_hamiltonian_ising}
\alias{diff_flipped_hamiltonian_ising}
\title{Diff in Energy of Generalized Ising Model by Single Flip}
\usage{
diff_flipped_hamiltonian_ising(u, j, h, J = NULL)
}
\arguments{
\item{u}{a binary ({-1, 1}-valued) vector or matrix. If \code{u} is a matrix, each
row is taken as an input vector.}

\item{j}{the (column) index of the variable to be flipped over}

\item{h}{a vector giving the external field.}

\item{J}{a symmetric matrix with zero diagonals giving the interaction. If
\code{J} is \code{NULL}, the model degenerates to the case of independent binary.}
}
\value{
a vector giving the energy difference
}
\description{
\code{diff_flipped_hamilton_ising} is a fast-computation method for energy
difference, used in the case where only one variable is flipped.
}
\details{
\deqn{\Delta_j E(u \vert h, J) = H(\tilde{u}; h, J) - H(u; h, J),}
where \eqn{\tilde{u}_j = - u_j} and \eqn{\tilde{u}_{-j} = u_{-j}}. It can be
shown that \eqn{\Delta_j E(u \vert h, J) = 2 u_j (h_j + J_{j, -j} u_{-j})}.
}
\examples{
h <- c(2, 3)
J <- matrix(c(0, -1, -1, 0), 2, 2)
u <- c(-1, +1)
diff_flipped_hamiltonian_ising(u, 2, h, J)

u <- matrix(c(-1, +1, +1, -1), 2, 2, byrow = TRUE)
diff_flipped_hamiltonian_ising(u, 1, h, J)
}
