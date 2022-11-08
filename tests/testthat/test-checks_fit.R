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

test_that("a matrix for r must have correct dimensions (n by 2)", {
  r <- matrix(c(1, 2, 5,
                2, 1, 1),
              ncol = 3, byrow = TRUE)
  expect_error(check_r(r), "columns")
})

test_that("`r` must be valid quantiles", {
  expect_error(check_rz(r = c(.2, 3),
                        z = 1), "so the values of")
})

test_that("thin must be TRUE or FALSE", {
  expect_error(check_thin("a"), "TRUE or FALSE")
})

test_that("p_select must be a valid choice", {
  expect_error(hystar_fit(y = 1, z = 3, p_select = "bla"),
               "aic")
})
