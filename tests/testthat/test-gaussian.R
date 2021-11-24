test_that("gaussian_model works as expected", {
  mu <- c(1, 2, 3)
  sigma <- matrix(c(2, -1, 0, -1, 2, 0.1, 0, 0.1, 3), 3, 3)
  gm <- gaussian_mdl(mu, sigma)

  n <- 10000
  X <- generate_data(gm, n)

  expect_equal(colMeans(X), mu, tolerance = 10 / sqrt(n))
  expect_equal(var(X), sigma, tolerance = 10 / sqrt(n))

  X_minus_mu <- X - matrix(mu, n, length(mu), byrow = TRUE)
  expect_equal(sum(log_prob(gm, X)),
               - 0.5 * (n * (3 * log(2 * pi) + log(det(sigma))) +
                          sum((t(X_minus_mu) %*% X_minus_mu) * chol2inv(chol(sigma)))))

  expect_equal(dof(gm), 9)

  gm_fit <- fit(gm, X)
  expect_true(bic(gm_fit, X) < bic(gm, X))
})
