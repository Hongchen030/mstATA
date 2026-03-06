#' @keywords internal
#' @noRd
.invert_tcc_scores <- function(scores, theta_grid, tcc, tol = 1e-4,range_tcc) {

  idx <- findInterval(x = scores, vec = tcc, rightmost.closed = TRUE)
  inv_tcc<-numeric(length(scores))

  for(i in seq_along(idx)){
    current_idx<-idx[i]
    if(current_idx==0){
      inv_tcc[i]<-range_tcc[1]
    }else if(current_idx>=length(theta_grid)){
      inv_tcc[i]<-range_tcc[2]
    }else{
      inv_tcc[i]<-.invert_tcc_score(scores[i], theta_grid, tcc, current_idx, tol)
    }
  }
  return(inv_tcc)
}

.invert_tcc_score <- function(score, theta_grid, tcc, idx, tol = 1e-4) {

  lb <- theta_grid[idx]
  ub <- theta_grid[idx + 1]

  t_lb <- tcc[idx]
  t_ub <- tcc[idx + 1]

  while(abs(lb-ub)>tol) {

    mid <- (lb + ub) / 2

    t_mid <- t_lb + (mid - lb) / (ub - lb) * (t_ub - t_lb)

    if (t_mid < score) {
      lb <- mid
      t_lb <- t_mid
    } else {
      ub <- mid
      t_ub <- t_mid
    }
  }

  (lb + ub) / 2
}
