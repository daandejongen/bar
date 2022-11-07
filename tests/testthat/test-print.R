z <- z_sim(n_t = 200, n_switches = 4, start_regime = 1)
sim <- hystar_sim(z = z, r = c(-.5, .5), d = 1,
                  phi_R0 = c(0, .4), phi_R1 = c(1, .5))
fit <- hystar_fit(y = sim$data$y, z = z)

test_that("printing does not raise error", {
  expect_error(print(fit))
})

