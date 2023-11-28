.onAttach <- function(libname, pkgname) {
hystar_string <- "
   __            __
  / /_ __ ______/ /_________
 / _  / // (_ -/  _/ _  / __\\
/_//_/\\_, /___)\\__/\\_,_/_/
     /___/             1.2.0

Estimation and simulation of the HysTAR Model.
For function help, run `?hystar_fit`, `?hystar_sim` or `?z_sim`.
For more information, run `hystar_info()` (opens a URL in your browser).
"

packageStartupMessage(hystar_string)
}

#' @export
#' @importFrom utils browseURL
hystar_info <- function() {
  browseURL("https://daandejongen.github.io/hystar/index.html")
}




