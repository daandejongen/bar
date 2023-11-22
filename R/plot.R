#' @export
plot.hystar_fit <- function(x, y = NULL, main = "Fitted HysTAR model", ...) {
  plot_hystar(hystar_object = x, main = main, ...)
  invisible()
}

#' @export
plot.hystar_sim <- function(x, y = NULL, main = "Simulated HysTAR model", ...) {
  plot_hystar(hystar_object = x, main = main, ...)
  invisible()
}

#' @importFrom graphics par
plot_hystar <- function(hystar_object,
                        main,
                        regimes_name_color = c("Regime 0" = "white",
                                               "Regime 1" = "grey85",
                                               "Unknown" = "#AA001137"),
                        ...) {
  old <- par(mfrow = c(2, 1))
  on.exit(par(old), add = TRUE)
  plot_z(hystar_object, main, regimes_name_color, ...)
  plot_y(hystar_object, regimes_name_color, ...)
}

#' @importFrom graphics axis
plot_z <- function(hystar_object,
                   main,
                   regimes_name_color,
                   zlab = "z",
                   color_z = "grey25",
                   line_type_z = 1,
                   line_width_z = 2.5,
                   show_legend = TRUE,
                   ...) {
  old <- par(mar = c(0, 4.1, 4.1, 2.1))
  on.exit(par(old), add = TRUE)
  plot(
    x = hystar_object$data$z,
    panel.first = c(
      plot_background_z(hystar_object, regimes_name_color),
      plot_threshold_lines(thresholds = hystar_object$thresholds, ...)
    ),
    type = "l",
    col = color_z,
    lty = line_type_z,
    lwd = line_width_z,
    xaxt = "n", xlab = "n",
    yaxt = "n", ylab = zlab,
    main = main
  )
  axis(side = 2, at = hystar_object$thresholds, labels = hystar_object$thresholds, las = 0)
  if (show_legend)
    plot_legend(hystar_object = hystar_object, regimes_name_color = regimes_name_color)
}

plot_y <- function(hystar_object,
                   regimes_name_color,
                   xlab = "time", ylab = "y",
                   color_y = "grey25",
                   line_type_y = 1,
                   line_width_y = 2.5,
                   ...) {
  old <- par(mar = c(4.1, 4.1, 0, 2.1))
  on.exit(par(old), add = TRUE)
  plot(x = hystar_object$data$y,
       panel.first = plot_background_y(hystar_object, regimes_name_color),
       type = "l",
       xlab = xlab,
       ylab = ylab,
       col = color_y,
       lty = line_type_y,
       lwd = line_width_y
  )
}

# Helpers -------------------------------------------------------------------------------

#' @importFrom graphics legend
plot_legend <- function(hystar_object, regimes_name_color) {
  n_unknown <- sum(is.na(hystar_object$data$R))
  if (n_unknown == 0) regimes_name_color <- regimes_name_color[-3]
  starting_regime <- hystar_object$data$R[!is.na(hystar_object$data$R)][1]
  legend_position <- if (starting_regime == 1) "bottomleft" else "topleft"
  legend(
    x = legend_position,
    legend = names(regimes_name_color),
    fill = regimes_name_color,
    bg = "grey95",
    cex = .6
  )
}

plot_background_z <- function(hystar_object, regimes_name_color) {
  plot_background(hystar_object, regimes_name_color, type = "z")
}

plot_background_y <- function(hystar_object, regimes_name_color) {
  plot_background(hystar_object, regimes_name_color, type = "y")
}

#' @importFrom graphics rect
plot_background <- function(hystar_object, regimes_name_color, type) {
  # We draw all the rectangles for the background color of Regime 1 at once.
  switch_points_matrix <- get_switch_points_matrix(regime_series = hystar_object$data$R)
  time_series <- if (type == "y") hystar_object$data$y else hystar_object$data$z
  plot_region_borders <- get_plot_region_borders(time_series = time_series)
  # Background coloring for regime 0
  rect(
    xleft = plot_region_borders["xleft"],
    ybottom = plot_region_borders["ybottom"],
    xright = plot_region_borders["xright"],
    ytop = plot_region_borders["ytop"],
    col = regimes_name_color[[1]],
    border = NA
  )
  # Background coloring for regime 1
  rect(
    xleft = switch_points_matrix[, 1],
    ybottom = rep(plot_region_borders["ybottom"], times = nrow(switch_points_matrix)),
    xright = switch_points_matrix[, 2],
    ytop = rep(plot_region_borders["ytop"], times = nrow(switch_points_matrix)),
    col = regimes_name_color[[2]],
    border = NA
  )
  # Background for the unknown regimes:
  # k + 1 is the first effective observation, so the rectangle must stop there.
  # only needed for hystar_fit object
  k <- sum(is.na(hystar_object$data$R))
  if (k > 0) {
    rect(
      xleft = 1,
      ybottom = plot_region_borders["ybottom"],
      xright = k + 1,
      ytop = plot_region_borders["ytop"],
      col = regimes_name_color[[3]],
      border = NA
    )
  }
}

#' @importFrom graphics abline
plot_threshold_lines <- function(thresholds,
                                 line_type_thresholds = 2,
                                 line_width_thresholds = 1,
                                 color_thresholds = "black",
                                 ...) {
  abline(h = thresholds,
         lty = line_type_thresholds,
         lwd = line_width_thresholds,
         col = color_thresholds)
}

get_plot_region_borders <- function(time_series) {
  # When drawing the rectangles for regimes, we want them to cover
  # the whole y axis. By default, R adds 4% of the plotted range
  # to the max value to avoid plotting it at the border.
  return(c(xleft = 1 - 0.04 * (length(time_series) - 1),
           ybottom = min(time_series) - 0.04 * (max(time_series) - min(time_series)),
           xright = length(time_series) + 0.04 * (length(time_series) - 1),
           ytop = max(time_series) + (max(time_series) - min(time_series)) * .04))
}

get_switch_points_matrix <- function(regime_series) {
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
  n <- length(regime_series)
  switch_points <- get_switch_points(regime_series)
  n_sw <- length(switch_points)
  even <- n_sw %% 2 == 0
  starts_with_1 <- regime_series[!is.na(regime_series)][1] == TRUE
  # These correspond to the four cases above.
  if (even) {
    if (starts_with_1)  points <- c(1, switch_points, n)
    if (!starts_with_1) points <- switch_points
  }
  if (!even) {
    if (starts_with_1)  points <- c(1, switch_points)
    if (!starts_with_1) points <- c(switch_points, n)
  }

  return(matrix(points, ncol = 2, byrow = TRUE))
}

get_switch_points <- function(regime_series) {
  # Where are switches from 0 (1) to 1 (0)?
  # We define a switch point as the time point where R is different
  # from R at the previous time point.
  # We don't want to use a slow for loop, so we vectorize the problem.
  # Note that at a switch point s, R[s] - R[s-1] != 0.
  # We compute S[t] = R[t] - R[t-1] and see were this is nonzero.
  # We use the lagged version of R, so we add the first time
  # point again (in which there can be no switch by definition).
  n <- length(regime_series)
  S <- regime_series[2:n] - regime_series[1:(n-1)]
  # Note that we have a vector that is one value too short, because
  # we lagged R. At the first time point, there can be no switch,
  # so we add that to the start.
  sw_points <- which(c(FALSE, S) != 0)

  return(sw_points)
}
