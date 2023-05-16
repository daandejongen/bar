test_that("search grid for r is correct", {
  z <- 1:3
  grid_r_actual <- create_grid_r(z, r = c(0, 1), thin = FALSE, tar = FALSE)
  grid_r_expected <- matrix(c(1.5, 2.5,
                              1.5, 1.5,
                              2.5, 2.5),
                            ncol = 2, byrow = TRUE)

  expect_equal(grid_r_actual, grid_r_expected)
})

test_that("d is correctly added", {
  grid <- matrix(nrow = 2, ncol = 2)
  grid_d_expected <- cbind(rep(c(1, 3), each = 2),
                           matrix(nrow = 4, ncol = 2))
  grid_d_actual <- add_d(d = c(1, 3), grid)

  expect_equal(grid_d_actual, grid_d_expected)
})

test_that("starts are correct", {
  y <- numeric(50)
  r0 <- 2
  r1 <- 4

  # When delay is larger than max order
  # (straightforward case, because you just look at first value of z)
  d <- 4
  eff <- time_eff(y, d, p0 = 1, p1 = 3) # returns 5, 6, ..., 50

  expect_equal(object = get_start(c(d, r0, r1), z = c(1, 3, 3, 3, 1), eff),
               expected = 0L)

  expect_equal(object = get_start(c(d, r0, r1), z = c(5, 3, 5, 3, 1), eff),
               expected = 1L)

  expect_equal(object = get_start(c(d, r0, r1), z = c(3, 5, 5, 5, 5), eff),
               expected = -1L)

  # When delay is smaller than max order
  # z contains observations at start that can inform about the first regime
  d <- 1
  eff <- time_eff(y, d, p0 = 1, p1 = 4) # returns 5, 6, ..., 50

  expect_equal(object = get_start(c(d, r0, r1), z = c(1, 3, 3, 3, 3), eff),
               expected = 0L)

  # Start is immediately known
  expect_equal(object = get_start(c(d, r0, r1), z = c(5, 3, 3, 1, 1), eff),
               expected = 0L)

  expect_equal(object = get_start(c(d, r0, r1), z = c(3, 3, 3, 3, 3), eff),
               expected = -1L)
})

test_that("z values are correct (with no thinning)", {
  z <- 1:10
  z_values <- get_z_values(z, r = c(.1, .9), thin = FALSE)
  expect_equal(z_values, seq(2.5, 8.5, 1))
})









