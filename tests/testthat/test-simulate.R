test_that("one step AR simulation has correct length", {
  y <- AR(x = 1:2, coe = c(0, .5), resvar = 1)
  expect_equal(length(y), 1)
})

test_that("burn-in samples has correct length", {
  y <- create_burn_in(50, c(0, .5), 1)
  expect_equal(length(y), 50)
})

test_that("y simulation has correct length", {
  R <- c(rep(0, 10), rep(1, 10))
  y <- y_sim(R, phi_R0 = 0, phi_R1 = 1, resvar = c(1, 1))
  expect_equal(length(y), 20)
})

test_that("sine/cosine simulation starts at right place", {
  z <- simulate_cossin(n_t = 3, n_switches = 1,
                       start_regime = 0, start_hyst = FALSE)
  expect_equal(z[11], -1) # There is a buffer of 10

  z <- simulate_cossin(n_t = 3, n_switches = 1,
                       start_regime = 1, start_hyst = FALSE)
  expect_equal(z[11], 1)

  z <- simulate_cossin(n_t = 3, n_switches = 1,
                       start_regime = 0, start_hyst = TRUE)
  expect_equal(z[11], 0)
})

test_that("z range is right", {
  z <- z_sim(n_t = 200, n_switches = 2, start_regime = 1, range = c(3, 8))
  expect_equal(min(z), 3)
  expect_equal(max(z), 8)
})




