print_welcome_message <- function() {
hystar_string <- "
 _                  _
| |                | |
| |__   _   _  ___ | |_   __ _  _ __
| '_  \\| | | |/ __||  _| / _` || '__\\
| | | || |_| |\\__ \\| |_ | (_| || |
|_| |_||___  ||___/ \\__| \\__,_||_|
        ___| |
       |____/                    1.2.0 \n\n"

info_string <- "
Estimation and simulation of the HysTAR Model.
For function help, run `?hystar_fit`, `?hystar_sim` or `?z_sim`.
For more information, run `hystar_info()`.
"
cat(hystar_string)
cat(info_string)
}

#' @export
#' @importFrom utils browseURL
hystar_info <- function() {
  browseURL("https://daandejongen.github.io/hystar/index.html")
}




