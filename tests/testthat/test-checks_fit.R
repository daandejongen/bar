test_that("unequal length of y and z throws error", {
  y <- numeric(1)
  z <- numeric(2)
  expect_error(check_yz(y, z), "must be of equal length")
})

test_that("a vector for r must be of correct length (2)", {
  expect_error(check_r(r = 1:3), "its length must be 2")
})

test_that("a matrix for r must have correct thresholds", {
  r <- matrix(c(1, 2,
                2, 1),
              nrow = 2, byrow = TRUE)
  expect_error(check_r(r), "The second threshold value should")
})

test_that("`r` must be valid quantiles", {
  expect_error(check_rz(r = c(.2, 3),
                        z = 1), "so the values of")
})

