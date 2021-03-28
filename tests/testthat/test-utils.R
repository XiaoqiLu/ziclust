test_that("standard logistic function is correct", {
  expect_equal(logistic(0), 0.5)
  expect_equal(logistic(-Inf), 0)
  expect_equal(logistic(Inf), 1)
})

test_that("rowwise kronecker function is correect", {
  x <- matrix(
    c(
      1, 1,
      1, 2,
      1, 3
    ),
    3, 2, byrow = TRUE
  )
  y <- matrix(
    c(
      1, 0,
      0, 1,
      1, 1
    ),
    3, 2, byrow = TRUE
  )
  expect_equal(
    rowwise_kronecker(x, y),
    matrix(
      c(
        1, 1, 0, 0,
        0, 0, 1, 2,
        1, 3, 1, 3
        ),
      3, 4, byrow = TRUE
    )
  )
})
