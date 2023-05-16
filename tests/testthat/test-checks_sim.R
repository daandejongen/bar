test_that("z should be numeric", {
  expect_error(hystar_sim(z = "hi", r = c(1, 2), d = 1, phi_R0 = 1, phi_R1 = 1),
               "numeric")
})

test_that("zero or negative residual variances are not allowed", {
  expect_error(check_resvar(c(0, 2)))
})

test_that("there should be 2 resvars", {
  expect_error(check_resvar(c(0, 2, 1)))
})

test_that("Thresholds should be within the range of z.", {
  expect_error(hystar_sim(z = 1:10, r = c(1, 4), d = 1,
                          phi_R0 = 1, phi_R1 = 2),
               "inside the range of `z`")
})

test_that("Delay or regime order cannot be too large", {
  expect_error(hystar_sim(z = 1:3, r = c(2, 2.2), d = 3,
                          phi_R0 = 1, phi_R1 = 2),
               "than the length of `z`")
  expect_error(hystar_sim(z = 1:3, r = c(2, 2.2), d = 1,
                          phi_R0 = 1, phi_R1 = c(0, .1, .1, .1)),
               "than the length of `z`")
})

test_that("unit root or explosive simulation throws warning", {
  expect_warning(check_phi(c(0, .8, .2), R01 = 1), "unit root")
  expect_warning(check_phi(c(0, .8, .3), R01 = 1), "explosive")
})

test_that("you cannot request too many switches", {
  expect_error(z_sim(n_t = 20, n_switches = 20, start_regime = 1))
})

test_that("When z is simulated with hysteresis,
          a certain start will raise an error", {
  z <- z_sim(100, 5, start_regime = 0, start_hyst = TRUE)

  expect_error(hystar_sim(z = z, r = c(-.9, -.8), d = 1, phi_R0 = 0, phi_R1 = 0,
                          resvar = c(1, 1)), "an hysteretic start")
})

test_that("When z is not simulated, and there is no hysteresis,
          starting regime gives warning if it does not match", {
  z <- 1:10
  expect_warning( # r makes start regime certainly 0
    s <- hystar_sim(z = z, r = c(2, 4), d = 1, phi_R0 = 0, phi_R1 = 0,
                    start_regime = 1),
    "`start_regime` is different from what is implied"
    )
  expect_equal(s$data$R[1], 0)

  expect_warning( # r makes start regime certainly 0
    s <- hystar_sim(z = z, r = c(2, 4), d = 1, phi_R0 = 0, phi_R1 = 0,
                    start_regime = 0),
    NA
  )
  expect_equal(s$data$R[1], 0)
})

test_that("When z is not simulated, and there is hysteresis,
          starting regime must be provided.", {
  z <- c(5, 1:10)
  s <- hystar_sim(z = z, r = c(2, 8), d = 1, phi_R0 = 0, phi_R1 = 0,
                  start_regime = 1)
  expect_equal(s$data$R[1], 1)

  expect_error(hystar_sim(z = z, r = c(2, 8), d = 1, phi_R0 = 0, phi_R1 = 0),
               "`start_regime` was not provided")
})

test_that("start check will return inferred start if incorrect start was provided,
          and warn that start_regime was ignored", {
  expect_warning(start <- check_hystar_sim_input(z = 1:5, r = c(2, 3), d = 1,
                                  phi_R0 = 0, phi_R1 = 0, resvar = c(1, 1),
                                  start_regime = 9)$start
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


