test_that("switch points are correct", {
  R <- c(0, 1, 1, 0, 1, 0, 0, 0, 1)
  true <- get_sw_pnts(R)
  exp <- c(2, 4, 5, 6, 9)
  expect_equal(true, exp)
})

