#' @keywords internal
#' @noRd
expand_A_real_parallel <- function(A_real_single, num_panels) {

  if (is.null(A_real_single)) {
    return(NULL)
  }

  if (!inherits(A_real_single, "Matrix")) {
    stop("`A_real_single` must be a Matrix object.", call. = FALSE)
  }

  if (num_panels == 1L) {
    return(A_real_single)
  }

  A_real_mp <- do.call(
    rbind,
    rep(list(A_real_single), num_panels)
  )

  if (!inherits(A_real_mp, "sparseMatrix")) {
    A_real_mp <- Matrix::Matrix(A_real_mp, sparse = TRUE)
  }

  return(A_real_mp)
}
