#' @keywords internal
#' @noRd
#'
Pi_grm<-function(theta,itempar_mat,model = c("GRM","MGRM"),D = 1){
  model <- match.arg(model,choices = c("GRM","MGRM"))
  if (model == "GRM") {
    max_cat<-ncol(itempar_mat)
  }else{
    max_cat<-ncol(itempar_mat)-1
  }
  prov  <- prov1 <- prov2 <- prov3 <- matrix(NA_real_, nrow(itempar_mat), max_cat)
  for (i in seq_len(nrow(itempar_mat))) {
    aj <- itempar_mat[i, 1]
    if (model == "GRM"){
      bj <- itempar_mat[i, 2:ncol(itempar_mat)]
    }else {
      bj <- itempar_mat[i, 2] - itempar_mat[i, 3:ncol(itempar_mat)]
    }
    bj <- bj[!is.na(bj)]
    K  <- length(bj)                # number of thresholds
    # cumulative probabilities
    s <- plogis(D * aj * (theta - bj))
    Pjs <- c(1, s, 0)
    Da <- D*aj
    dPjs  <- c(0, Da * s * (1 - s), 0)
    # category probabilities
    prov[i,  1:(K + 1)] <- Pjs[1:(K + 1)] - Pjs[2:(K + 2)]
    prov1[i, 1:(K + 1)] <- dPjs[1:(K + 1)] - dPjs[2:(K + 2)]
  }
  colnames(prov) <- paste0("cat",0:(ncol(prov)-1))

  res <- list(Pi = prov, dPi = prov1)
  return(res)
}

