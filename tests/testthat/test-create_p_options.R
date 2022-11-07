test_that("p options are correct", {
  p <- create_p_options(p0 = c(1, 3), p1 = c(2, 4))
  e <- matrix(c(1, 2, NA, NA, NA, NA, NA,
                1, 4, NA, NA, NA, NA, NA,
                3, 2, NA, NA, NA, NA, NA,
                3, 4, NA, NA, NA, NA, NA),
              ncol = 7, byrow = TRUE)
  colnames(e) <- c("p0", "p1", "d", "r0", "r1", "s", "ic")

  expect_equal(p, e)
})


