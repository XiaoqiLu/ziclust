% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clustering.R
\name{mixture_mdl}
\alias{mixture_mdl}
\title{Create an object of S3 class \code{mixture_mdl}}
\usage{
mixture_mdl(tau, components, hyper_params = list())
}
\arguments{
\item{tau}{a vector giving the weights of the mixture model}

\item{components}{a list of mixture components, each is a \code{mdl} object}

\item{hyper_params}{hyperparameters (not implemented)}
}
\value{
object of class \code{mixture_mdl}
}
\description{
Create an object of S3 class \code{mixture_mdl}
}
\examples{
gm1 <- gaussian_mdl(c(1, 2), matrix(c(1, 0, 0, 2), 2, 2))
gm2 <- gaussian_mdl(c(-5, -3), matrix(c(2, -1, -1, 1), 2, 2))
mixture_mdl(c(0.2, 0.8), list(gm1, gm2))
}
