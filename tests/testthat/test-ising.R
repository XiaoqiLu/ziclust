test_that("Hamiltanion of Ising is correct", {
  expect_equal(
    hamiltonian_ising(
      u = c(-1, +1),
      h = c(2, 3),
      J = matrix(0, 2, 2)
    ),
    -1
  )
})
