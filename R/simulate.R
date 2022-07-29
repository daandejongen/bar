simulate <- function(y, eff, R, phi, psi, resvar) {

  for (i in eff) {

    if (R[i] == 0L) {
      x <- lag_obs(y, i, p0)
      y[i] <- AR(x, phi, resvar[1])
    }

    if (R[i] == 1L) {
      x <- lag_obs(y, i, p1)
      y[i] <- AR(x, psi, resvar[2])
    }

  }

  return(y)
}




