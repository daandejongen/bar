#' @export
plot.bardata <- function(x, y = NULL, ...) {
  y <- x$y
  z <- x$z
  R <- x$R
  r <- attr(x, "r")
  n_ineff <- attr(x, "n_ineff")
  col_reg1 <- "grey80"

  time <- 1:length(y)
  # Set bottom margin for the top plot (z) to zero
  old <- par(mar = c(0, 4.1, 4.1, 2.1),
             mfrow = c(2, 1))
  on.exit(par(old), add = TRUE)

  sw_pnts_mat <- get_sw_pnts_mat(R)

  make_zplot(time, z, r, R, n_ineff, col_reg1, sw_pnts_mat)

  par(mar = c(4.1, 4.1, 0, 2.1))

  make_yplot(time, y, R, n_ineff, col_reg1, sw_pnts_mat)
}


make_zplot <- function(time, z, r, R, n_ineff, col_reg1, sw_pnts_mat) {
  plot(time, z,
       panel.first = c(rect_all(R, z, col_reg1, n_ineff, sw_pnts_mat),
                       abline(h = r, lty = 2, col = "black")),
       main = "BAR model", ylab = "z", xaxt = "n", yaxt = "n",
       type = "l", lwd = 2.5, col = "grey25")

  legend(x = get_minmax(time)[1], y = get_minmax(z)[2],
         legend = c(paste0("regime ", 0:1), "not pred."),
         fill = c("white", col_reg1, "#AA001137"),
         bg = "grey95"
  )

  axis(side = 2, at = r, labels = c("r0", "r1"), las = 2)
}


make_yplot <- function(time, y, R, n_ineff, col_reg1, sw_pnts_mat) {
  plot(time, y,
       panel.first = rect_all(R, y, col_reg1, n_ineff, sw_pnts_mat),
       xlab = "time", ylab = "y",
       type = "l", lwd = 2.5, col = "grey25")

  axis(2)
  axis(1)
}


rect_all <- function(R, x, col_reg1, n_ineff, sw_pnts_mat) {
  rect_bg(R, x, col_reg1)
  rect_reg(R, x, col_reg1, sw_pnts_mat)
  rect_ineff(x, n_ineff)
}


rect_ineff <- function(x, n_ineff) {
  rect(xleft = 1, ybottom = get_minmax(x)[1],
       xright = n_ineff, ytop = get_minmax(x)[2],
       col = "#AA001137", border = NA)
}


rect_reg <- function(R, x, col_reg1, sw_pnts_mat) {
  # We want to always draw regime rectangles from 1 to 2, 3 to 4, ...
  # so the rectangle color is the color of on the starting regime
  # and we chose the background color the opposite color.
  rect_col <- c(col_reg1, "white")[2 - R[1]]

  ybottom <- rep(get_minmax(x)[1], times = nrow(sw_pnts_mat))
  ytop    <- rep(get_minmax(x)[2], times = nrow(sw_pnts_mat))

  rect(sw_pnts_mat[, 1], ybottom, sw_pnts_mat[, 2], ytop,
       col = rect_col, border = NA)
}



rect_bg <- function(R, x, col_reg1) {
  col_bg <- c("white", col_reg1)[2 - R[1]]

  rect(1, get_minmax(x)[1], length(R), get_minmax(x)[2],
       col = col_bg, border = NA)
}



get_minmax <- function(x) {
  # When drawing the rectangles for regimes, we want them to cover
  # the whole y axis. By default, R adds 4% of the plotted range
  # to the max value to avoid plotting it at the border.
  min <- min(x)
  max <- max(x)
  dist <- max - min

  return(c(min - dist*.04,
           max + dist*.04))
}


get_sw_pnts_mat <- function(R) {
  # Where are switches from 0 (1) to 1 (0)?
  # We use the lagged version of R, so we add the first time
  # point again (in which there can be no switch by definition).
  n <- length(R)
  sw_pnts <- get_sw_pnts(R)

  # We delete the last sw_point if there are an uneven number of them.
  # We put them in a matrix (by row) to have a from and a to column.
  n <- length(sw_pnts)
  even <- n %% 2 == 0
  points <- if (even) c(1, sw_pnts, length(R)) else  c(1, sw_pnts)

  sw_pnts_mat <- matrix(points, ncol = 2, byrow = TRUE)

  return(sw_pnts_mat)
}

