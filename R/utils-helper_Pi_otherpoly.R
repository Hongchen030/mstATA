#' @keywords internal
#' @noRd
#'
Pi_otherpoly<-function(theta,itempar_mat,model = c("PCM","GPCM","RSM","NRM"),D = 1){
  model <- match.arg(model,choices = c("PCM","GPCM","RSM","NRM"))

  if(model == "PCM"){
    max_cat<-ncol(itempar_mat)+1
  }
  if (model == "GPCM"){
    max_cat<-ncol(itempar_mat)
  }

  if (model == "RSM"){
    max_cat<-ncol(itempar_mat)
  }

  if(model=="NRM"){
    if (ncol(itempar_mat) %% 2 != 0){
      stop("NRM requires an even number of columns (a,b pairs).", call. = FALSE)
    }
    max_cat<-ncol(itempar_mat)/2+1
  }
  prov <- prov1 <- prov2 <- prov3 <- matrix(NA_real_,nrow = nrow(itempar_mat), ncol = max_cat)
  for (i in seq_len(nrow(itempar_mat))) {
    dj <- v <- 0
    if (model == "PCM") {
      for (t in 1:ncol(itempar_mat)) {
        if (is.na(itempar_mat[i, t])) break
        dj <- c(dj, dj[t] + D * (theta - itempar_mat[i, t]))
        v <- c(v, t)
      }
    }
    if (model == "GPCM") {
      a <- itempar_mat[i,1]
      for (t in 1:(ncol(itempar_mat) - 1)) {
        if (is.na(itempar_mat[i, t + 1])) break
        dj <- c(dj, dj[t] + a * D * (theta - itempar_mat[i, t + 1]))
        v <- c(v, a * t)
      }
    }
    if (model == "RSM") {
      b <- itempar_mat[i, 1]
      for (t in 1:(ncol(itempar_mat) - 1)) {
        if (is.na(itempar_mat[i, t + 1])) break
        dj <- c(dj, dj[t] + D * (theta - (b + itempar_mat[i, t + 1])))
        v <- c(v, t)
      }
    }
    if (model == "NRM") {
      K<-ncol(itempar_mat)/2
      for (t in seq_len(K)) {
        a_k<-itempar_mat[i, (2 * t - 1)]
        b_k<-itempar_mat[i, (2 * t)]
        if (is.na(a_k) || is.na(b_k)) break
        dj <- c(dj, a_k * theta + b_k)
        v <- c(v, a_k)
      }
    }
    Gammaj <- exp(dj)
    dGammaj <- Gammaj * v
    Sg <- sum(Gammaj)
    Sdg  <- sum(dGammaj)
    n <- length(Gammaj)
    prov[i, 1:n] <- Gammaj/Sg
    prov1[i, 1:n] <- dGammaj/Sg - Gammaj * Sdg/Sg^2
  }

  colnames(prov) <- paste0("cat",0:(ncol(prov)-1))
  res <- list(Pi = prov, dPi = prov1)
  return(res)
}
