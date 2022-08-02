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

grid_s_actual <- add_start(grid_d_actual, z, eff = 2:3)
grid_s_expected <- matrix(c(1, 1, 2, 0,
                            1, 1, 3, 0,
                            1, 2, 3, 9,
                            1, 1, 1, 9,
                            1, 2, 2, 9,
                            1, 3, 3, 9,
                            1, 1, 2, 1,
                            1, 1, 3, 1),
                          ncol = 4, byrow = TRUE)

test_that("search grid for r is correct", {
  expect_equal(grid_r_actual, grid_r_expected)
})

test_that("d is correctly added", {
  expect_equal(grid_d_actual, grid_d_expected)
})

test_that("s is correctly added", {
  expect_equal(grid_s_actual, grid_s_expected)
})


