test_that("hysteretic time series is correct", {
  actual   <- ts_hys(z = c(3, 1, 3, 5, 3), r0 = 2, r1 = 4)
  expected <- as.integer(c(-1, 0, -1, 1, -1))
  expect_equal(actual, expected)

  not_expected <- c(-1, 0, -1, 1, -1) # double
  !expect_equal(actual, not_expected)
})

test_that("regime time series is correct", {
  actual   <- ts_reg(as.integer(c(1, -1, 0, -1, 1)))
  expected <- as.integer(c(1, 1, 0, 0, 1))
  expect_equal(actual, expected)

  actual   <- ts_reg(as.integer(c(-1, 1, -1, 0, -1)), start = 0)
  expected <- as.integer(c(0, 1, 1, 0, 0))
  expect_equal(actual, expected)
})
