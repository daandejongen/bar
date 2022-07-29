test_that("non-numeric argument throws error", {
  expect_error(check_data(y = logical(3), z = numeric(3)),
               class = "not_numeric")
  expect_error(check_data(y = numeric(3), z = logical(3)),
               class = "not_numeric")

  expect_error(check_dp(d = "a", p0 = 1, p1 = 1),
               class = "not_numeric")
  expect_error(check_dp(d = 1, p0 = NA, p1 = 1),
               class = "not_numeric")
  expect_error(check_dp(d = 1, p0 = 2, p1 = data.frame(1)),
               class = "not_numeric")

  expect_error(check_r(r = "a"), class = "not_numeric")
})


test_that("unequal length of y and z throws error", {
  y <- numeric(1)
  z <- numeric(2)
  expect_error(check_data(y, z), "y and z must be of equal length")
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

test_that("search is of type char", {
  expect_error(check_search(TRUE), "an object of type character")
})

test_that("search is restricted to certain choices", {
  expect_error(check_search("blabla"), "search must be one of")
})

test_that("Simulation inputs must be valid", {
  z <- 1:10
  d <- 4
  r <- c(3, 6)
  phi <- c(.3, .1)
  psi <- c(.6, .2, 0)
  resvar <- c(2, 3)
  init_vals <- 1:4
  start_regime <- NULL

  expect_error(check_sim_input(z, d, r, phi, psi = 2*psi, resvar,
                               init_vals, start_regime),
               "Autoregressive coefficients in 'phi'")

  expect_error(check_sim_input(z, d, r, phi, psi, resvar = -resvar,
                               init_vals, start_regime),
               "Residual variance")

  expect_error(check_sim_input(z, d, r, phi, psi, resvar = resvar,
                               init_vals = 1:3, start_regime),
               "Too few")

  expect_error(check_sim_input(z, d, r = c(-10, 20), phi, psi, resvar,
                               init_vals, start_regime),
               "A starting regime is needed")

})




