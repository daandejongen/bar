#' @export
#' @importFrom graphics abline
#' @importFrom graphics axis
#' @importFrom graphics legend
#' @importFrom graphics par
#' @importFrom graphics rect
#' @importFrom graphics segments
plot.hystar_fit <- function(x, y = NULL,
                            main = "Fitted HysTAR model",
                            show_legend = TRUE,
                            ...) {
  # Both the hystar_fit class and the hystar_sim class have a
  # plot method, but they both call plot_hystar().
  # The difference is in the main of the plot and that
  # hystar_sim has no ineffective observations (k).

  plot_hystar(y = x$data$y,
              z = x$data$z,
              R = x$data$R,
              r = x$thresholds,
              k = sum(is.na(x$data$R)),
              main = main,
              show_legend = show_legend,
              tar = x$tar,
              ...)

  invisible()
}

#' @export
plot.hystar_sim <- function(x, y = NULL,
                            main = "Simulated HysTAR model",
                            show_legend = TRUE,
                            ...) {

  plot_hystar(y = x$data$y,
              z = x$data$z,
              R = x$data$R,
              r = x$r,
              k = 0,
              main = main,
              show_legend = show_legend,
              tar = x$tar,
              ...)

  invisible()
}

plot_hystar <- function(y, z, R, r, k, main, show_legend, tar, ...) {
  # The plot is built as follows: first a z plot is made
  # and then a y plot is made. The par() function makes
  # sure that they appear on top of each other.
  col_reg1 <- "grey80"
  time <- 1:length(y)

  # Set bottom margin for the top plot (z) to zero.
  # If you don't use on.exit(), the changes to the options
  # of the user will persist (so new plots will follow this
  # two row grid) and we don't want that.
  old <- par(mar = c(0, 4.1, 4.1, 2.1), mfrow = c(2, 1))
  on.exit(par(old), add = TRUE)
  # The switch points are needed for the background plot of
  # the regimes. It is made in such a way that we always make
  # background filled rectangles for Regime 1. The rectangles
  # are used in the z plot and the y plot.
  sw_pnts_mat <- get_sw_pnts_mat(R)

  make_zplot(time, z, r, R, k, col_reg1, sw_pnts_mat, main, show_legend, ...)
  # Margin at top is set to zero so the plots will "touch".
  par(mar = c(4.1, 4.1, 0, 2.1))

  make_yplot(time, y, R, k, col_reg1, sw_pnts_mat, ...)
}

make_zplot <- function(time, z, r, R, k, col_reg1, sw_pnts_mat, main,
                       show_legend, regime_names = c("Regime 0", "Regime 1"),
                       zlab = "z", ...) {
  args <- list(...)

  plot(time, z,
       # panel.first ensures that the rectangles are at the background.
       panel.first = c(
         rect_reg(x = z, col_reg1, sw_pnts_mat),
         rect_ineff(x = z, k = k),
         abline(h = r, lty = 2, col = "black")
         ),
       # The x-axis is made eventually in the y plot. For the y-axis, we
       # only want the threshold values, those are made by axis().
       main = main,
       ylab = zlab,
       xaxt = "n", yaxt = "n",
       type = "l",
       lwd = 2.5,
       col = "grey25")
  axis(side = 2, at = r, labels = c("r0", "r1"), las = 2, cex = .5)

  # The legend depends on whether there are ineffective observations or not.
  # With no ineff obs, we only want the colors of the Regimes in the legend.
  names <- if (k > 0) c(regime_names, "Unpred.") else regime_names
  fills <- if (k > 0) c("white", col_reg1, "#AA001137") else c("white", col_reg1)
  legend_up_lo <- if (R[!is.na(R)][1] == 1) "bottomleft" else "topleft"
  if (show_legend)
    legend(x = legend_up_lo, legend = names, fill = fills, bg = "grey95", cex = .6)
}

make_yplot <- function(time, y, R, k, col_reg1, sw_pnts_mat,
                       xlab = "time", ylab = "y", ...) {
  args <- list(...)

  plot(time, y,
       panel.first = c(
         rect_reg(x = y, col_reg1, sw_pnts_mat),
         rect_ineff(x = y, k = k)
         ),
       xlab = xlab,
       ylab = ylab,
       yaxt = "n",
       type = "l",
       lwd = 2.5,
       col = "grey25")
  axis(side = 2, las = 2)
}

rect_reg <- function(x, col_reg1, sw_pnts_mat) {
  # We draw all the rectangles for Regime 1 at once.
  # The switch points matrix provide the x values and
  # we use the min and max (plus 4 percent) for the y values.
  ybottom <- rep(get_minmax(x)[1], times = nrow(sw_pnts_mat))
  ytop    <- rep(get_minmax(x)[2], times = nrow(sw_pnts_mat))
  rect(sw_pnts_mat[, 1], ybottom, sw_pnts_mat[, 2], ytop,
       col = col_reg1,
       border = NA)
}

rect_ineff <- function(x, k) {
  # k + 1 is the first effective observation, so the rectangle must
  # stop there.
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
  # This function will return a matrix with two columns, the first column
  # represents time points where Regime 1 starts (again) and the second
  # column represents values where it stops. The tricky thing is that we
  # can start in Regime 1 or not, and that there can be an even or an uneven
  # number of switches. These are the situations (--- means coloring, : is
  # start or end, * is a switch point).
  # This is an example where the switch points are 5, 10, 15, (20).
  #                         time 1   5  10  15  20  25
  # R[1] = 1 & n_switch = even   :---*   *---*   *---:
  # R[1] = 0 & n_switch = even   :   *---*   *---*   :
  # R[1] = 1 & n_switch = uneven :---*   *---*   :
  # R[1] = 0 & n_switch = uneven :   *---*   *---:
  # For the first case, we want a matrix:
  #  1  5
  # 10 15
  # 20 25
  n <- length(R)
  sw_pnts <- get_sw_pnts(R)
  n_sw <- length(sw_pnts)
  even <- n_sw %% 2 == 0
  starts_with_1 <- R[!is.na(R)][1] == TRUE

  # These correspond to the four cases above.
  if (even) {
    if (starts_with_1)  points <- c(1, sw_pnts, n)
    if (!starts_with_1) points <- sw_pnts
  }
  if (!even) {
    if (starts_with_1)  points <- c(1, sw_pnts)
    if (!starts_with_1) points <- c(sw_pnts, n)
  }

  sw_pnts_mat <- matrix(points, ncol = 2, byrow = TRUE)

  return(sw_pnts_mat)
}

get_sw_pnts <- function(R) {
  # Where are switches from 0 (1) to 1 (0)?
  # We define a switch point as the time point where R is different
  # from R at the previous time point.
  # We don't want to use a slow for loop, so we vectorize the problem.
  # Note that at a switch point s, R[s] - R[s-1] != 0.
  # We compute S[t] = R[t] - R[t-1] and see were this is nonzero.
  # We use the lagged version of R, so we add the first time
  # point again (in which there can be no switch by definition).
  n <- length(R)
  S <- R[2:n] - R[1:(n-1)]
  # Note that we have a vector that is one value too short, because
  # we lagged R. At the first time point, there can be no switch,
  # so we add that to the start.
  sw_points <- which(c(FALSE, S) != 0)

  return(sw_points)
}

