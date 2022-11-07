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
})

