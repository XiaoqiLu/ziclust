test_that("Hamiltanion of Ising is correct", {
  expect_equal(
    hamiltonian_ising(
      u = c(-1, +1),
      h = c(2, 3),
      J = matrix(0, 2, 2)
    ),
    -1
  )
  expect_equal(
    hamiltonian_ising(
      u = c(+1, +1),
      h = c(0, 0),
      J = matrix(c(0, -1, -1, 0), 2, 2)
    ),
    1
  )
  expect_equal(
    hamiltonian_ising(
      u = matrix(c(-1, +1, +1, -1), 2, 2, byrow = TRUE),
      h = c(2, 3),
      J = matrix(0, 2, 2)
    ),
    c(-1, 1)
  )
})

test_that("fast computation of diff in Hamilton (flipping one variable) is correct", {
  h <- c(2, 3)
  J <- matrix(c(0, -1, -1, 0), 2, 2)

  u <- c(-1, +1)
  u_flipped <- c(-1, -1) # j = 2
  energy_diff <- hamiltonian_ising(u_flipped, h, J) - hamiltonian_ising(u, h, J)
  expect_equal(
    diff_flipped_hamiltonian_ising(u, 2, h, J),
    energy_diff
  )

  u <- matrix(c(-1, +1, +1, -1), 2, 2, byrow = TRUE)
  u_flipped <- matrix(c(+1, +1, -1, -1), 2, 2, byrow = TRUE) # j = 1
  energy_diff <- hamiltonian_ising(u_flipped, h, J) - hamiltonian_ising(u, h, J)
  expect_equal(
    diff_flipped_hamiltonian_ising(u, 1, h, J),
    energy_diff
  )
})
