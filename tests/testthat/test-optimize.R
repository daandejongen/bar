test_that("Correct starting regime is selected
          in more than 90 percent of the cases", {
  z <- sin(1:100 * .1)
  d <- 2
  phi <-  c(-.5, -.3)
  psi <-  c(.5, .5, .5)
  p0 <- get_order(phi)
  p1 <- get_order(psi)
  r0 <- -.5
  r1 <- .5
  start <- 0

  correct <- logical(98)

  for (i in 1:98) {
    y <- barsim(z, d = 2, r = c(r0, r1), phi, psi,
                resvar = c(.2, .2), start_regime = start)
    x <- create_x(y, d, p0, p1)
    eff <- time_eff(y, d, p0, p1)
    y_eff <- y[eff]
    z_del <- z[eff - d]
    H <- ts_hys(z_del, r0, r1)
    expected <- ts_reg(H, start = start)
    actual <- optim_start(y_eff, x, p0, p1, H)
    correct[i] <- all(expected == actual)
  }

  expect_gt(sum(correct), 90)
})


test_that("Rs matrix is as expected", {
  y <- 1:8
  eff <- time_eff(y, 2, 2, 2)
  y_eff <- y[eff]
  z <- c(0, 0, 5, 5, 16, 30, 30, 30)
  grid <- matrix(c(1, 10, 20,
                   1, 2, 4,
                   2, 10, 20,
                   2, 4, 10), nrow = 4, ncol = 3, byrow = TRUE)
  colnames(grid) <- c("d", "r0", "r1")
  actual <- get_Rs(y_eff, eff, x = 1, z = z, p0 = 2, p1 = 2, grid = grid)
  expected <- matrix(c(0, 0, 0, 0, 1, 1,
                       0, 1, 1, 1, 1, 1,
                       0, 0, 0, 0, 0, 1,
                       0, 0, 0, 0, 1, 1),
                     nrow = 4, ncol = 6, byrow = TRUE)
  expect_equal(actual, expected)
})



