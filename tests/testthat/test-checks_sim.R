test_that("zero residual variances are not allowed", {
  expect_error(check_resvar(c(0, 2)))
})

test_that("unit root simulation throws error", {
  expect_error(check_phi(c(0, .8, .2), R01 = 1))
})

test_that("start check will throw error if start is unknown and no start was provided", {
  expect_error(check_hystar_sim_input(z = 1:5, r = c(-0, 7), d = 1,
                                  phi_R0 = 0, phi_R1 = 0, resvar = c(1, 1),
                                  init_vals = NULL, start_regime = NULL),
               "The starting regime is unknown")
})

test_that("start check will return inferred start if no start was provided", {
  start <- check_hystar_sim_input(z = 1:5, r = c(6, 7), d = 1,
                                  phi_R0 = 0, phi_R1 = 0, resvar = c(1, 1),
                                  init_vals = NULL, start_regime = NULL)
  expect_equal(start, 0)
})

test_that("start check will return inferred start if incorrect start was provided,
          and warn that start_regime was ignored", {
  expect_warning(start <- check_hystar_sim_input(z = 1:5, r = c(6, 7), d = 1,
                                  phi_R0 = 0, phi_R1 = 0, resvar = c(1, 1),
                                  init_vals = NULL, start_regime = 9)
  )
  expect_equal(start, 0)
})

test_that("invalid start regime throws error", {
  expect_error(check_start(start_regime = 4, start_inferred = -1))
})



