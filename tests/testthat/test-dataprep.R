test_that("lagged matrix is correct", {
  # Simple case
  y <- 1:6
  n <- length(y)
  p <- 3
  eff <- select_obs(n = n, d = 2, p = p)$eff
  actual <- create_x_2(y = y, eff = eff, p = p)

  expected <- matrix(c(1, 3, 2, 1,
                       1, 4, 3, 2,
                       1, 5, 4, 3),
                     nrow = 3, byrow = TRUE)
  expect_equal(actual, expected)

  # The edge case where the max order is zero
  expect_equal(create_x(y = 1:5, p = 0),
               matrix(1, nrow = 5, ncol = 1))
})

test_that("y_eff and z_del are equally long", {
  n <- 100
  d <- 5
  p <- 3
  x <- select_obs(n, d, p)
  expect_equal(length(x$eff), length(x$del))
})


test_that("search grid is correct", {
  actual   <- create_grid(z = 1:3, r_bounds = c(1, 3), search = "custom")
  expected <- matrix(c(1, 2,
                       1, 3,
                       2, 3,
                       1, 1,
                       2, 2,
                       3, 3),
                     ncol = 2, byrow = TRUE)
  expect_equal(actual, expected)

  actual_2 <- create_grid(z = 3:1, r_bounds = c(0, 1), search = "quantile")
  expect_equal(actual_2, expected)
})







