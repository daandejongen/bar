test_that("y_eff and z_del are equally long", {
  y <- 1:100
  d <- c(2, 3, 5)
  p0 <- 3
  p1 <- 2
  eff <- time_eff(y, d, p0, p1)
  del <- time_del(y, d, p0, p1, d_sel = 3)
  expect_equal(length(eff), length(del))
})

