#' @keywords internal
#' @noRd
Pi_dichotomous<-function(theta,itempar_mat,D = 1){
  a <- itempar_mat[, 1]
  b <- itempar_mat[, 2]
  c <- itempar_mat[, 3]
  d <- itempar_mat[, 4]
  z <- D * a * (theta - b)
  # Stable logistic
  L <- plogis(z)
  # Guard against exact 0/1 for downstream divisions
  L <- pmin(pmax(L, 1e-10), 1-1e-10)
  Pi  <- c + (d - c) * L
  dPi <- (d - c) * D * a * (L * (1 - L))
  res <- list(Pi = Pi, dPi = dPi)
  return(res)
}
