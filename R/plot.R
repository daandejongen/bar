#' @export
#' @importFrom graphics abline
#' @importFrom graphics axis
#' @importFrom graphics legend
#' @importFrom graphics par
#' @importFrom graphics rect
#' @importFrom graphics segments
plot.hystar_fit <- function(x, y = NULL, ...) {
  plot_hystar(y = x$data$y,
              z = x$data$z,
              R = x$data$R,
              r = x$thresholds,
              k = sum(is.na(x$data$R)))

  invisible()
}

#' @export
plot.hystar_sim <- function(x, y = NULL, ...) {
  plot_hystar(y = x$data$y,
              z = x$data$z,
              R = x$data$R,
              r = x$r,
              k = 0)

  invisible()
}

plot_hystar <- function(y, z, R, r, k) {
  col_reg1 <- "grey80"
  time <- 1:length(y)
  # Set bottom margin for the top plot (z) to zero
  old <- par(mar = c(0, 4.1, 4.1, 2.1),
             mfrow = c(2, 1))
  on.exit(par(old), add = TRUE)
  sw_pnts_mat <- get_sw_pnts_mat(R)
  make_zplot(time, z, r, R, k, col_reg1, sw_pnts_mat)
  # Set top margin for the bottom plot (y) to zero
  par(mar = c(4.1, 4.1, 0, 2.1))
  make_yplot(time, y, R, k, col_reg1, sw_pnts_mat)
}

make_zplot <- function(time, z, r, R, k, col_reg1, sw_pnts_mat) {
  plot(time, z,
       panel.first = c(rect_reg(R, x = z, col_reg1, sw_pnts_mat),
                       rect_ineff(x = z, k = k),
                       abline(h = r, lty = 2, col = "black")),
       main = "HysTAR model", ylab = "z", xaxt = "n", yaxt = "n",
       type = "l", lwd = 2.5, col = "grey25")

  regs <- paste0("Regime ", 0:1)
  names <- if (k > 0 ) c(regs, "unpred.") else regs
  fills <- if (k > 0 ) c("white", col_reg1, "#AA001137") else c("white", col_reg1)
  legend(x = get_minmax(time)[1], y = get_minmax(z)[2],
         legend = names, fill = fills, bg = "grey95")

  axis(side = 2, at = r, labels = c("r0", "r1"), las = 2)
}

make_yplot <- function(time, y, R, k, col_reg1, sw_pnts_mat) {
  plot(time, y,
       panel.first = c(rect_reg(R, x = y, col_reg1, sw_pnts_mat),
                       rect_ineff(x = y, k = k)),
       xlab = "time", ylab = "y",
       type = "l", lwd = 2.5, col = "grey25")
  axis(2)
  axis(1)
}

rect_reg <- function(R, x, col_reg1, sw_pnts_mat) {
  # We want to always draw regime rectangles from 1 to 2, 3 to 4, ...
  # so the rectangle color is the color of on the starting regime
  # and we chose the background color the opposite color.

  ybottom <- rep(get_minmax(x)[1], times = nrow(sw_pnts_mat))
  ytop    <- rep(get_minmax(x)[2], times = nrow(sw_pnts_mat))

  rect(sw_pnts_mat[, 1], ybottom, sw_pnts_mat[, 2], ytop,
       col = col_reg1, border = NA)
}

rect_ineff <- function(x, k) {
  rect(xleft = 1, ybottom = get_minmax(x)[1],
       xright = k + 1, ytop = get_minmax(x)[2],
       col = "#AA001137", border = NA)
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
  n <- length(R)
  sw_pnts <- get_sw_pnts(R)

  # We delete the last sw_point if there are an uneven number of them.
  # We put them in a matrix (by row) to have a from and a to column.
  n_sw <- length(sw_pnts)
  even <- n_sw %% 2 == 0
  start_with_1 <- R[!is.na(R)][1] == TRUE

  if (even) {
    if (start_with_1) points <- c(1, sw_pnts, n)
    if (!start_with_1) points <- sw_pnts
  }
  if (!even) {
    if (start_with_1) points <- c(1, sw_pnts)
    if (!start_with_1) points <- c(sw_pnts, n)
  }

  sw_pnts_mat <- matrix(points, ncol = 2, byrow = TRUE)

  return(sw_pnts_mat)
}

get_sw_pnts <- function(R) {
  # Where are switches from 0 (1) to 1 (0)?
  # We use the lagged version of R, so we add the first time
  # point again (in which there can be no switch by definition).
  n <- length(R)
  return(which(c(FALSE, R[2:n] - R[1:(n-1)] != 0)))
}

