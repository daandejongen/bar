.onAttach <- function(libname, pkgname) {
packageStartupMessage("
   __            __
  / /_ __ ______/ /_________
 / _  / // (_ -/  _/ _  / __\\
/_//_/\\_, /___)\\__/\\_,_/_/
     /___/             1.2.0

Estimation and simulation of the HysTAR Model.
For function help, run `?hystar_fit`, `?hystar_sim` or `?z_sim`.
For more information, run `hystar_info()` (opens a URL in your browser).
")
}

#' Get more information about the hystar package
#'
#' @description directs you to the hystar website
#' https://daandejongen.github.io/hystar/index.html
#' @return Nothing
#' @export
#' @importFrom utils browseURL
hystar_info <- function() {
  browseURL("https://daandejongen.github.io/hystar/"); invisible(NULL)
}




