test_that("lagged matrix is correct", {
  # Simple case
  y <- 1:6
  p0 <- 3
  p1 <- 1
  d <-  2

  actual <- create_x(y, d, p0, p1)

  expected <- matrix(c(1, 3, 2, 1,
                       1, 4, 3, 2,
                       1, 5, 4, 3),
                     nrow = 3, byrow = TRUE)
  expect_equal(actual, expected)

  # The edge case where the max order is zero
  expect_equal(create_x(y = 1:5, d = 0, p0 = 0, p1 = 0),
               matrix(1, nrow = 5, ncol = 1))
})

test_that("y_eff and z_del are equally long", {
  y <- 1:100
  d <- c(2, 3, 5)
  p0 <- 3
  p1 <- 2
  eff <- time_eff(y, d, p0, p1)
  del <- time_del(y, d, p0, p1, d_sel = 3)
  expect_equal(length(eff), length(del))
})









