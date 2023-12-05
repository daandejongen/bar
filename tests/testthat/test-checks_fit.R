test_that("users should provide data", {
  expect_error(hystar_fit(),
               "Argument `data` is missing, with no default.")
})

test_that("data must be numeric", {
  expect_error(hystar_fit(data.frame(letters, letters)),
               "numeric")
})

test_that("data must be a vector, matrix or df", {
  expect_error(hystar_fit(mean),
               "vector, matrix or data.frame")
})

test_that("data cannot have missings", {
  expect_error(hystar_fit(c(1:20, NA)),
               "not have missing values")
})

test_that("r must be numeric", {
  expect_error(check_r_fit(letters), "numeric")
})

test_that("r must be a proper interval", {
  expect_error(check_r_fit(c(3, 2)), "interval")
})

test_that("a vector for r must be of correct length (2)", {
  expect_error(check_r_fit(r = 1:3), "its length must be 2")
})

test_that("a matrix for r must have correct thresholds", {
  r <- matrix(c(1, 2,
                2, 1),
              nrow = 2, byrow = TRUE)
  expect_error(check_r_fit(r), "The second threshold value should")
})

test_that("a matrix for r must have correct thresholds if TAR is specified", {
  r <- matrix(c(1, 2,
                3, 3),
              nrow = 2, byrow = TRUE)
  expect_error(check_r_fit(r, tar = TRUE), "TAR")
})

test_that("user is notified that they are fitting a TAR model", {
  r <- matrix(c(2, 2,
                3, 3),
              nrow = 2, byrow = TRUE)
  expect_warning(check_r_fit(r, tar = FALSE), "TAR")
})

test_that("a matrix for r must have correct dimensions (n by 2)", {
  r <- matrix(c(1, 2, 5,
                2, 1, 1),
              ncol = 3, byrow = TRUE)
  expect_error(check_r_fit(r), "columns")
})

test_that("`r` must be valid quantiles", {
  expect_error(check_rz(r = c(.2, 3),
                        z = 1), "so the values of")
})

test_that("`r` must be in the range of `z`", {
  expect_error(check_rz(r = matrix(c(2, 20), nrow = 1),
                        z = 1:10), "range")
})

test_that("`r` must be numeric", {
  expect_error(hystar_fit(hystar_sim(z_sim()$data, r = c("a", "b"))))
})

test_that("thin must be TRUE or FALSE", {
  expect_error(check_thin("a"), "TRUE or FALSE")
})

test_that("tar must be TRUE or FALSE", {
  expect_error(check_tar(2), "TRUE or FALSE")
})

test_that("show_progress must be TRUE or FALSE", {
  expect_error(check_show_progress(2), "TRUE or FALSE")
})

test_that("p_select must be a valid choice", {
  expect_error(hystar_fit(1:100, p_select = 1),
               "character")
  expect_error(hystar_fit(data.frame(y = 1:10, z = 1:10), p_select = "bla"),
               "aic")
  p_select <- check_hystar_fit_input(
    z = 1:4, d = 1, p0 = 1, p1 = 1,
    p_select = "aic", r = c(.8, .9), thin = TRUE, tar = FALSE, show_progress = TRUE
    )
  expect_equal(p_select, "aic")
})

test_that("`z` must have at least three levels.", {
  expect_error(hystar_fit(data.frame(y = 1:10,
                                     z = c(rep(3, 5), rep(4, 5))
                                     )
                          ), "unique values")
})

test_that("things run normally with certain arguments off-default", {
  expect_no_error({
    hystar_fit(hystar_sim(z_sim())$data, show_progress = TRUE)
  })
})

test_that("fit throws no errors with certain off-default settings", {
  expect_no_error({
    hystar_fit(1:100, show_progress = TRUE)
  })
})

