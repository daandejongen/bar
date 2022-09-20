#' @export
plot.bardata <- function(x, y = NULL, ...) {
  y <- x$y
  z <- x$z
  R <- x$R
  r <- attr(x, "r")
  n_ineff <- attr(x, "n_ineff")

  time <- 1:length(y)
  # Set bottom margin for the top plot (z) to zero
  old <- par(mar = c(0, 4.1, 4.1, 2.1),
             mfrow = c(2, 1))
  on.exit(par(old), add = TRUE)
  make_zplot(time, z, r, R, n_ineff)

  par(mar = c(4.1, 4.1, 0, 2.1))
  make_yplot(time, y, R, n_ineff)
}


make_zplot <- function(time, z, r, R, n_ineff) {
  plot(time, z,
       panel.first = c(ineff_rect(n_ineff, min(z), max(z)),
                       abline(h = r, lty = 2, col = "black")),
       main = "BAR model", ylab = "z", xaxt = "n", yaxt = "n",
       type = "l", col = "grey50")
  points(z, pch = 20, col = regime_colors(R), xaxt = "n")
  legend(x = 1, y = max(z),
         legend = paste0("regime ", 0:1),
         col = c("royalblue2", "red3"),
         pch = 20
  )
  axis(side = 2, at = r, labels = c("r0", "r1"), las = 2)
}


make_yplot <- function(time, y, R, n_ineff) {
  plot(time, y,
       panel.first = ineff_rect(n_ineff, min(y), max(y)),
       xlab = "time", ylab = "y",
       type = "l", col = "grey50")
  points(time, y, pch = 20, col = regime_colors(R))
  axis(2)
  axis(1)
}


regime_colors <- function(R, col_0 = "royalblue2", col_1 = "red3") {
  return(ifelse(R == 0, col_0, col_1))
}


ineff_rect <- function(n_ineff, min, max) {
  rect(xleft = 1, ybottom = min, xright = n_ineff, ytop = max,
       col = "#AA001137", border = NA)
}


