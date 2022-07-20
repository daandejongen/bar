test_that("non-numeric argument throws error", {
  expect_error(check_data(y = logical(3), z = numeric(3)),
               class = "not_numeric")
  expect_error(check_data(y = numeric(3), z = logical(3)),
               class = "not_numeric")

  expect_error(check_dp(d = "a", p0 = 1, p1 = 1),
               class="not_numeric")
  expect_error(check_dp(d = 1, p0 = NA, p1 = 1),
               class="not_numeric")
  expect_error(check_dp(d = 1, p0 = 2, p1 = data.frame(1)),
               class="not_numeric")

  expect_error(check_r(r="a"), class="not_numeric")
})


test_that("unequal length of y and z throws error", {
  y <- numeric(1)
  z <- numeric(2)
  expect_error(check_data(y, z), "y and z must be of equal length")
})

test_that("delay cannot be larger than max order", {
  expect_error(check_dp(d = 5, p0 = 1:4, p1 = 3), "cannot exceed")
})

test_that("delay and orders should be whole numbers", {
  expect_error(check_dp(d = 1, p0 = 1.5, p1 = 9), "whole numbers")
})

test_that("r should contain two numbers", {
  expect_error(check_r(r = 1), "provide exactly two values")
  expect_error(check_r(r = 1:3), "provide exactly two values")
})

test_that("rL is smaller than rU", {
  expect_error(check_r(r = c(3, 2)), "The first threshold")
})

test_that("r must fall in the range of z", {
  r <- c(0, 10)
  z <- 2:9
  expect_error(check_r(r, z, method="custom"), "Threshold values must fall")
})

test_that("search is of type char", {
  expect_error(check_search(TRUE), "an object of type character")
})

test_that("search is restricted to certain choices", {
  expect_error(check_search("blabla"), "search must be one of")
})






