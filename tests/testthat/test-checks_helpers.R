test_that("whole numbers are correctly checked", {
  expect_true(is_whole(5))
  expect_false(is_whole(pi))
})

test_that("custom errors have correct variable name", {
  hey <- TRUE
  expect_error(error_numeric(hey), "`hey` must be numeric")
  mister <-  -9
  expect_error(error_nonnegative(mister), "`mister` must be nonnegative")
  blue <-  0.6
  expect_error(error_whole(blue), "`blue` must be a whole")
  skye <-  7
  expect_error(error_logical(skye), "`skye` must be TRUE or FALSE")
})
