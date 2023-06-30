z <- z_sim(n_t = 200, n_switches = 4, start_regime = 1)
sim <- hystar_sim(z = z, r = c(-.5, .5), d = 1,
                  phi_R0 = c(0, .4), phi_R1 = c(1, .5))
fit <- hystar_fit(sim$data)

test_that("coef method output has correct dimensions", {
  expect_equal(dim(coef(fit)), c(2, 2))
})

test_that("residuals and fitted values have correct length", {
  expect_equal(length(residuals(fit)), 199)
  expect_equal(length(fitted(fit)), 199)
})

test_that("confint method output has correct dimensions", {
  # Suppress warning about negative variances of estimators.
  suppressWarnings({
    expect_equal(dim(confint(fit)), c(4, 2))
  })
})

test_that("n obs is correct", {
  e <- c(199, sum(1 - fit$data$R, na.rm = TRUE), sum(fit$data$R, na.rm = TRUE))
  names(e) <- c("used", "regime0", "regime1")
  expect_equal(nobs(fit), e)
})




