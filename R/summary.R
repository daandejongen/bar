#' @export
summary.bar <- function(x) {
  n <- unname(attr(x, "n"))
  coe <- attr(x, "coe")

  opening <- paste0("BAR model fitted on ", n[1], " observations.\n
                    Of which ", n[1], " observations in regime 0 and ",
                    n[2], " observations in regime 1.\n\n")
  printcoe <- paste0("Model coefficients:\n", coe)

  cat(opening, printcoe)
}



