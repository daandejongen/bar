test_that("design matrix is correct", {
  x <- matrix(c(1, 1, 4,
                1, 2, 5,
                1, 3, 6),
              nrow = 3, byrow = TRUE)
  actual <- create_X(x = x, R = c(0L, 1L, 0L), p0 = 1, p1 = 2)
  expected <- matrix(c(1, 1, 0, 0, 0,
                       0, 0, 1, 2, 5,
                       1, 3, 0, 0, 0),
                     nrow = 3, byrow = TRUE)
  expect_equal(actual, expected)
})
