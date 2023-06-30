test_that("switch points are correct", {
  R <- c(0, 1, 1, 0, 1, 0, 0, 0, 1)
  exp <- c(2, 4, 5, 6, 9)
  expect_equal(get_sw_pnts(R), exp)
})

test_that("switch point matrix is correct. Start 1, uneven switches", {
  # switch points should be 2 3 4.
  R <- c(1, 0, 1, 0, 0)
  exp_mat <- matrix(c(1, 2,
                      3, 4),
                    ncol = 2, byrow = TRUE)
  expect_equal(get_sw_pnts_mat(R), exp_mat)
})

test_that("switch point matrix is correct. Start 1, even switches", {
  # switch points should be 2 3 4 5.
  R <- c(1, 0, 1, 0, 1, 0)
  exp_mat <- matrix(c(1, 2,
                      3, 4,
                      5, 6), # 6 is the last time point
                    ncol = 2, byrow = TRUE)
  expect_equal(get_sw_pnts_mat(R), exp_mat)
})

test_that("switch point matrix is correct. Start 0, uneven switches", {
  # switch points should be 2 3 4.
  R <- c(0, 1, 0, 1, 1, 1)
  exp_mat <- matrix(c(2, 3,
                      4, 6), # 6 is the last time point
                    ncol = 2, byrow = TRUE)
  expect_equal(get_sw_pnts_mat(R), exp_mat)
})

test_that("switch point matrix is correct. Start 0, even switches", {
  # switch points should be 2 3 4 5.
  R <- c(0, 1, 0, 1, 0, 0)
  exp_mat <- matrix(c(2, 3,
                      4, 5),
                    ncol = 2, byrow = TRUE)
  expect_equal(get_sw_pnts_mat(R), exp_mat)
})

test_that("plot function does not return", {
  z <- z_sim(n_t = 50, n_switches = 3,
             start_regime = 1, start_hyst = TRUE,
             range = c(-1, 1))
  sim <- hystar_sim(z = z, r = c(-.5, .5), d = 5,
                    phi_R0 = c(0, .6), phi_R1 = c(1. -.3),
                    resvar = c(1, .9),
                    start_regime = 1)
  fit <- hystar_fit(sim$data)

  expect_equal(plot(sim), NULL)
  expect_equal(plot(fit), NULL)
})

