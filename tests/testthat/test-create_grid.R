z <-  c(2, 1, 3)

grid_r_actual <- create_grid_r(z, r_bounds = c(1, 3), search = "scale")
grid_r_expected <- matrix(c(1, 2,
                            1, 3,
                            2, 3,
                            1, 1,
                            2, 2,
                            3, 3),
                          ncol = 2, byrow = TRUE)

grid_d_actual <- add_d(d = 1, grid = grid_r_actual)
grid_d_expected <- cbind(rep(1, each = 6), grid_r_expected)


test_that("search grid for r is correct", {
  expect_equal(grid_r_actual, grid_r_expected)
})

test_that("d is correctly added", {
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









