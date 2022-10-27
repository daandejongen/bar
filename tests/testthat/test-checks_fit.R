test_that("unequal length of y and z throws error", {
  y <- numeric(1)
  z <- numeric(2)
  expect_error(check_data(y, z), "must be of equal length")
})

test_that("delay and orders should be whole numbers", {
  expect_error(check_dp(d = 1, p0 = 1.5, p1 = 9), "whole numbers")
})

test_that("delay and orders should be non-negative", {
  expect_error(check_dp(d = 1, p0 = -1, p1 = 0), "non-negative")
})

test_that("invalid r throws correct error.", {
  expect_error(check_r_type("q", r = 1), "length must be 2")
  expect_error(check_r_type("q", r = matrix(1, ncol = 3)), "2 columns")
  expect_error(check_r_type("q", r = c(2, 1)), "larger than")
  expect_error(check_r_type("quar", r = c(1, 2)), "must be")
  expect_error(check_r_type("q", r = c(1, 2)), "so the values")
  expect_error(check_r_type("s", r = c(1, 9), z = 1:4), "so the values")
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




