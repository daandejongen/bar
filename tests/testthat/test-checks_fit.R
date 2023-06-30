test_that("a vector for r must be of correct length (2)", {
  expect_error(check_r_fit(r = 1:3), "its length must be 2")
})

test_that("a matrix for r must have correct thresholds", {
  r <- matrix(c(1, 2,
                2, 1),
              nrow = 2, byrow = TRUE)
  expect_error(check_r_fit(r), "The second threshold value should")
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

test_that("thin must be TRUE or FALSE", {
  expect_error(check_thin("a"), "TRUE or FALSE")
})

test_that("p_select must be a valid choice", {
  expect_error(hystar_fit(data.frame(y = 1:10, z = 1:10), p_select = 1),
               "character")
  expect_error(hystar_fit(data.frame(y = 1:10, z = 1:10), p_select = "bla"),
               "aic")
  p_select <- check_hystar_fit_input(
    z = 1:4, d = 1, p0 = 1, p1 = 1,
    p_select = "aic", r = c(.8, .9), thin = TRUE, tar = FALSE
    )
  expect_equal(p_select, "aic")
})


test_that("`z` must have at least three levels.", {
  expect_error(hystar_fit(data.frame(y = 1:10,
                                     z = c(rep(3, 5), rep(4, 5))
                                     )
                          ), "unique values")
})

