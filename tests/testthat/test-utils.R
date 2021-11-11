test_that("standard logistic function is correct", {
  expect_equal(logistic(0), 0.5)
  expect_equal(logistic(-Inf), 0)
  expect_equal(logistic(Inf), 1)
})

test_that("rowwise Kronecker function is correect", {
  x <- matrix(
    c(
      1, 1,
      1, 2,
      1, 3
    ),
    3, 2,
    byrow = TRUE
  )
  y <- matrix(
    c(
      1, 0,
      0, 1,
      1, 1
    ),
    3, 2,
    byrow = TRUE
  )

  got <- rowwise_kronecker(x, y)
  want <- matrix(
    c(
      1, 1, 0, 0,
      0, 0, 1, 2,
      1, 3, 1, 3
    ),
    3, 4,
    byrow = TRUE
  )

  expect_equal(got, want)
})

test_that("rng for Dirichlet distribution is correct (by checking moments)", {
  alphas <- c(2, 4, 6)

  alpha0 <- sum(alphas)
  z_expectation <- alphas / alpha0
  z_covariance <- (alpha0 * diag(alphas) - alphas %o% alphas) / (alpha0 ^ 2 * (alpha0 + 1))

  n <- 10000
  Z <- rdirichlet(n, alphas)
  z_mean <- colMeans(Z)
  z_sample_covariance <- var(Z)

  expect_equal(colMeans(Z), z_expectation, tolerance = 10 / sqrt(n))
  expect_equal(var(Z), z_covariance, tolerance = 10 / sqrt(n))
})
