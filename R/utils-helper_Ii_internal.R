#' Fisher information for an item
#'
#' Computes Fisher information at a given ability level.
#'
#' @param theta Scalar ability value.
#' @param itempar_mat Item parameter matrix.
#' @param model Item response model.
#' @param D Scaling constant.
#'
#' @return Numeric Fisher information.
#' @keywords internal
#' @noRd

Ii_internal<-function(theta, itempar_mat,model = NULL, D = 1){
  if (length(theta) != 1L){
    stop("Ii_internal() expects a scalar theta.", call. = FALSE)
  }
  itempar_mat<-as.matrix(itempar_mat)
  pr <- Pi_internal(theta = theta,itempar_mat = itempar_mat,model = model,D = D)
  P <- pr$Pi
  dP <- pr$dPi

  if(is.null(model)){
    Q <- 1 - P
    Ii <- dP^2/(P * Q)
   }else{
    model<- match.arg(model,choices = c("GRM","MGRM","PCM","GPCM","RSM","NRM"))
    pr0 <- dP^2/P
    Ii <- as.numeric(rowSums(pr0, na.rm = TRUE))
   }
  names(Ii)<-NULL
  return(Ii)
}


