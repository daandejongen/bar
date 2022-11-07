test_that("r with the smallest distance is selected", {
  optims <- matrix(c(1, 1, 2, 0,
                     1, 1, 3, 0),
                   nrow = 2, byrow = TRUE)
  colnames(optims) <- c("d", "r0", "r1", "s")

  expected <- matrix(c(1, 1, 2, 0),
                     nrow = 1, byrow = TRUE)
  colnames(expected) <- c("d", "r0", "r1", "s")

  expect_equal(select_r(optims), expected)
})

test_that("minimal delay is selected", {
  optims <- matrix(c(2, 1, 2, 0,
                     1, 1, 3, 0),
                   nrow = 2, byrow = TRUE)
  colnames(optims) <- c("d", "r0", "r1", "s")

  expected <- matrix(c(1, 1, 3, 0),
                     nrow = 1, byrow = TRUE)
  colnames(expected) <- c("d", "r0", "r1", "s")

  expect_equal(select_min_d(optims), expected)
})




