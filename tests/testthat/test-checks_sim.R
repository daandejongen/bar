test_that("z should be numeric", {
  expect_error(check_z("lalala"), "numeric")
})

test_that("zero or negative residual variances are not allowed", {
  expect_error(check_resvar(c(0, 2)))
})

test_that("there should be 2 resvars", {
  expect_error(check_resvar(c(0, 2, 1)))
})

test_that("unit root or explosive simulation throws warning", {
  expect_warning(check_phi(c(0, .8, .2), R01 = 1), "unit root")
  expect_warning(check_phi(c(0, .8, .3), R01 = 1), "explosive")
})

test_that("you cannot request too many switches", {
  expect_error(z_sim(n_t = 20, n_switches = 20, start_regime = 1))
})

test_that("start check will throw error if start is unknown and no start was provided", {
  expect_error(check_hystar_sim_input(z = 1:5, r = c(-0, 7), d = 1,
                                      phi_R0 = 0, phi_R1 = 0, resvar = c(1, 1),
                                      start_regime = NULL),
               "The starting regime is unknown")
})

test_that("start check will return inferred start if no start was provided", {
  start <- check_hystar_sim_input(z = 1:5, r = c(6, 7), d = 1,
                                  phi_R0 = 0, phi_R1 = 0, resvar = c(1, 1),
                                  start_regime = NULL)
  expect_equal(start, 0)
})

test_that("start check will return inferred start if incorrect start was provided,
          and warn that start_regime was ignored", {
  expect_warning(start <- check_hystar_sim_input(z = 1:5, r = c(6, 7), d = 1,
                                  phi_R0 = 0, phi_R1 = 0, resvar = c(1, 1),
                                  start_regime = 9)
  )
  expect_equal(start, 0)
})

test_that("start_hyst must be TRUE or FALSE", {
  expect_error(check_start_hyst(NA), "TRUE or FALSE")
  expect_error(check_start_hyst("a"), "TRUE or FALSE")
})

test_that("range input must be valid", {
  expect_error(z_sim(100, 4, 1, FALSE, c(3, 1), "not larger than the first"))
  expect_error(z_sim(100, 4, 1, FALSE, 3, "a vector of length 2"))
})


