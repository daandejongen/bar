test_that("lagged matrix is correct", {
  # Simple case
  y <- 1:6
  p0 <- 3
  p1 <- 1
  eff <- 4:6
  actual <- create_x(y, eff, p0, p1)

  expected <- matrix(c(1, 3, 2, 1,
                       1, 4, 3, 2,
                       1, 5, 4, 3),
                     nrow = 3, byrow = TRUE)
  expect_equal(actual, expected)
})

test_that("lagged matrix is also correct when p=0 and d=0", {
  expect_equal(create_x(y = 1:5, eff = 1:5, p0 = 0, p1 = 0),
               matrix(1, nrow = 5, ncol = 1))
})

test_that("X is correctly formed", {
  p0 <- 1
  p1 <- 2
  x <- create_x(y = 1:5, eff = 3:5, p0 = p0, p1 = p1)
  R <- c(0, 1, 0)
  X_true <- create_X(x = x, p0 = p0, p1 = p1, R = R)
  X_exp <- matrix(c(1, 2, 0, 0, 0,
                    0, 0, 1, 3, 2,
                    1, 4, 0, 0, 0),
                  nrow = 3, byrow = TRUE)
  expect_equal(X_true, X_exp)
})




