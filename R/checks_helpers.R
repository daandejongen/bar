is_whole <- function(x, tol = .Machine$double.eps) {
  whole <- all(abs(round(x) - x) < tol)
  return(whole)
}

error_whole <- function(x) {
  err <- structure(
    list(
      message = paste0("`", substitute(x), "` must be a whole number."),
      call = NULL
    ),
    class = c("not_whole", "error", "condition")
  )
  stop(err)
}

error_numeric <- function(x) {
  err <- structure(
    list(
      message = paste0("`", substitute(x), "` must be numeric. ",
                       "You provided an object of type ",
                       typeof(x), "."),
      call = NULL
    ),
    class = c("not_numeric", "error", "condition")
  )
  stop(err)
}

error_nonnegative <- function(x) {
  err <- structure(
    list(
      message = paste0("`", substitute(x), "` must be nonnegative."),
      call = NULL
    ),
    class = c("not_nonnegative", "error", "condition")
  )
  stop(err)
}

error_logical <- function(x) {
  err <- structure(
    list(
      message = paste0("`", substitute(x), "` must be TRUE or FALSE ",
                       "You provided an object of type ",
                       typeof(x), ". Namely: `", x, "`."),
      call = NULL
    ),
    class = c("not_logical", "error", "condition")
  )
  stop(err)
}




