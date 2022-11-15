test_that("y_eff and z_del are equally long", {
  y <- 1:100
  d <- c(2, 3, 5)
  p0 <- 3
  p1 <- 2
  eff <- time_eff(y, d, p0, p1)
  del <- time_del(y, d, p0, p1, d_sel = 3)
  expect_equal(length(eff), length(del))
})

test_that("number of effective obs is correct", {
  eff <- time_eff(1:100, d = 3, p0 = 5, p1 = 2)
  expect_equal(length(eff), 95)
})

test_that("lagged observations are correct", {
  x1 <- lag_obs(y = 1:10, t = 4, p = 3)
  expect_equal(x1, c(1, 3, 2, 1))

  # Edge case with zero order process.
  x2 <- lag_obs(y = 1:10, t = 4, p = 0)
  expect_equal(x2, 1)
})

test_that("order is correct", {
  order <- get_order(coe = c(0, .4, .5))
  expect_equal(order, 2)

  order <- get_order(coe = c(0, .5, 0, 0))
  expect_equal(order, 1)

  order <- get_order(coe = c(0, 0, 0, 0))
  expect_equal(order, 0)
})

test_that("Trailing zeros are correctly removed.", {
  # Note that by default we want to keep the first value,
  # regardless of its value!
  expect_equal(remove_trailing_zeros(c(1, 2, 0, 0)),
               c(1, 2))

  expect_equal(remove_trailing_zeros(c(0, 0, 1, 3)),
               c(0, 0, 1, 3))

  expect_equal(remove_trailing_zeros(c(0, 0, 0, 0)),
               0)
})

test_that("z buffer is correctly removed, d > p.", {
  z <- z_sim(n_t = 50, n_switches = 3, start_regime = 1, start_hyst = TRUE)
  z_r <- remove_z_buffer(z, d = 3, p0 = 1, p1 = 2)
  expect_equal(z_r[1], 0)
  expect_equal(z_r[1:50], z[11:60])
})

test_that("z buffer is correctly removed, d < p.", {
  z <- z_sim(n_t = 50, n_switches = 3, start_regime = 1, start_hyst = FALSE)
  z_r <- remove_z_buffer(z, d = 2, p0 = 1, p1 = 5)
  expect_equal(z_r[5 + 1 - 2], 1)
})

test_that("z buffer removal keeps original attributes.", {
  z <- z_sim(30, 5, 1)
  z_r <- remove_z_buffer(z, d = 2, p0 = 2, p1 = 1)

  expect_equal(attributes(z), attributes(z_r))
})



