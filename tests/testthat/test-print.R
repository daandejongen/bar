test_that("intercept string is correct", {
  expect_equal(get_string_intercept(-3), "- 3 ")
  expect_equal(get_string_intercept(3), "3 ")
  expect_equal(get_string_intercept(0), "")
})

test_that("printed formula of AR process is correct", {
  expect_equal(make_formula(coe = c(-2, 4, 0, 1, -5), rv = 1),
               "- 2 + 4 y[t-1] + y[t-3] - 5 y[t-4] + e[t]")

  expect_equal(make_formula(coe = c(0, 0, 0, 0, 1), rv = 1),
               "y[t-4] + e[t]")

  expect_equal(make_formula(coe = c(0, 2, 0, -1, 2), rv = 2),
               "2 y[t-1] - y[t-3] + 2 y[t-4] + 2 e[t]")

  expect_equal(make_formula(coe = c(0), rv = 1),
               "e[t]")
})

test_that("summary and print do not return something", {
  z <- z_sim(n_t = 200, n_switches = 3,
             start_regime = 1, start_hyst = TRUE,
             range = c(-1, 1))
  sim <- hystar_sim(z = z, r = c(-.5, .5), d = 5,
                    phi_R0 = c(0, .6), phi_R1 = c(1. -.3),
                    resvar = c(1, .9),
                    start_regime = 1)
  fit <- hystar_fit(sim$data)

  expect_equal(summary(sim), NULL)
  expect_equal(summary(fit), NULL)
  expect_equal(print(sim), NULL)
  expect_equal(print(fit), NULL)
})




