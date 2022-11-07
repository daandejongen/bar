test_that("whole numbers are correctly checked", {
  expect_true(is_whole(5))
  expect_false(is_whole(pi))
})

test_that("negative whole numbers raise error", {
  expect_error(check_whole_nn(-5))
})

test_that("whole error has correct variable name", {
  hey <- 4.4
  expect_error(error_whole(hey), "`hey` must be a whole number")
})

test_that("numeric error has correct variable name", {
  hey <- TRUE
  expect_error(error_numeric(hey), "`hey` must be numeric")
})

test_that("nonnegative error has correct variable name", {
  hey <- -9
  expect_error(error_nonnegative(hey), "`hey` must be nonnegative")
})
