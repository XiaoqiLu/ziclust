test_that("Hamiltanion of Ising is correct", {
  # test for vector case
  expect_equal(
    hamiltonian_ising(
      u = c(+1, +1),
      h = c(0, 0),
      J = matrix(
        c(
          0, -1,
          -1, 0
          ),
        2, 2, byrow = TRUE
      )
    ),
    1
  )
  # test for matrix case
  expect_equal(
    hamiltonian_ising(
      u = matrix(
        c(
          -1, +1,
          +1, -1
          ),
        2, 2, byrow = TRUE
        ),
      h = c(2, 3),
      J = matrix(0, 2, 2)
    ),
    c(-1, 1)
  )
  # test for J is NULL case
  expect_equal(
    hamiltonian_ising(
      u = matrix(
        c(
          -1, +1,
          +1, -1
        ),
        2, 2, byrow = TRUE
      ),
      h = c(2, 3)
    ),
    c(-1, 1)
  )
})

test_that("fast computation of diff in Hamilton (flipping one variable) is correct", {
  h <- c(2, 3)
  J <- matrix(
    c(
      0, -1,
      -1, 0
      ),
    2, 2, byrow = TRUE
  )
  # test for vector case
  u <- c(-1, +1)
  u_flipped <- c(-1, -1) # j = 2
  energy_diff <- hamiltonian_ising(u_flipped, h, J) - hamiltonian_ising(u, h, J)
  expect_equal(
    diff_flipped_hamiltonian_ising(u, 2, h, J),
    energy_diff
  )
  # test for matrix case
  u <- matrix(
    c(
      -1, +1,
      +1, -1
      ),
    2, 2, byrow = TRUE
  )
  u_flipped <- matrix(
    c(
      +1, +1,
      -1, -1
      ),
    2, 2, byrow = TRUE
  ) # j = 1
  energy_diff <- hamiltonian_ising(u_flipped, h, J) - hamiltonian_ising(u, h, J)
  expect_equal(
    diff_flipped_hamiltonian_ising(u, 1, h, J),
    energy_diff
  )
  # test for J is NULL case
  energy_diff <- hamiltonian_ising(u_flipped, h) - hamiltonian_ising(u, h)
  expect_equal(
    diff_flipped_hamiltonian_ising(u, 1, h),
    energy_diff
  )
})

test_that("Ising gibbs sampler is working reasonably (field only)", {
  significance_level <- 0.01
  n <- 1000
  p <- 3
  h <- rnorm(p)
  J <- matrix(0, p, p)
  u <- matrix(2 * (stats::runif(n * p) < 0.5) - 1, n, p)
  u_gibbs <- gibbs_ising(u, h, J)
  prob <- (1 + tanh(h)) / 2 # for J = 0 case
  x <- (n + colSums(u_gibbs)) / 2
  test <- prop.test(x, rep(n, p), p = prob)
  if (test$p.value < significance_level) {
    warning(paste("reject null at significance level =", significance_level))
  }
  expect_true(TRUE)
})
