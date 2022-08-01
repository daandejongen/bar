new_bar <- function(optim, eff, y, x, z, p0, p1) {

  regnames <- paste0("regime", 0:1)

  R    <- optim$R_est
  X    <- create_X(x, p0, p1, R)
  fit  <- fit(y, X)
  coe  <- coe_to_matrix(fit$coe, p0, p1)
  rv   <- estimate_resvar(R, fit$res)
  names(rv) <- regnames

  zdrH <- get_zdrH(optim, eff, z)

  orders <- c(p0, p1)
  names(orders) <- regnames

  rss <- fit$rss
  names(rss) <- "rss"

  n <- c(length(y), sum(1-R), sum(R))
  names(n) <- c("total", regnames)

  bar <- structure(
    list(),
    class   = "bar",
    y_eff   = y,
    eff     = eff,
    x       = x,
    z_del   = zdrH$z,
    H       = zdrH$H,
    R       = R,
    res     = fit$res,
    d       = zdrH$d,
    r       = zdrH$r,
    coe     = coe,
    resvar  = rv,
    orders  = orders,
    rss     = fit$rss,
    n       = n
    )

  return(bar)
}


# Helpers

get_zdrH <- function(optim, eff, z) {
  # For the selection of r0 and r1 in the matrix of estimates, we
  # can just select the first row, since they yield equivalent R
  # time series and thus identical results.
  d     <- optim$est[1, "d", drop = TRUE]
  z_del <- z[eff - d]

  r     <- optim$est[, c("r0", "r1"), drop = FALSE]
  f     <- function(z, r) ts_hys(z, r[1L], r[2L])
  H     <- t(apply(r, 1, FUN = f, z = z_del))

  return(list(z_del = z_del, d = d, r = r, H = H))
}


coe_to_matrix <- function(coe, p0, p1) {
  nc <- 1L + max(p0, p1)

  zeros0 <- rep(0L, nc - (p0 + 1L))
  zeros1 <- rep(0L, nc - (p1 + 1L))

  coe0 <- c(coe[1:(1 + p0)], zeros0)
  coe1 <- c(coe[(p0+2):(p0+p1+2)], zeros1)

  C <- matrix(c(coe0, coe1), nrow = 2, byrow = TRUE)

  colnames(C) <- c("constant", paste0("Lag ", 1:(nc-1)))
  rownames(C) <- paste0("regime", 0:1)

  return(C)
}






