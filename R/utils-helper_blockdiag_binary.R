#' @keywords internal
#' @noRd
blockdiag_binary <- function(A_binary_single, num_panels) {
  if (!inherits(A_binary_single, "Matrix")) {
    stop("`A_binary_single` must be a Matrix object.", call. = FALSE)
  }

  if (num_panels == 1L) {
    return(A_binary_single)
  }

  blocks <- rep(list(A_binary_single), num_panels)

  A_binary_mp <- Matrix::bdiag(blocks)

  if (!inherits(A_binary_mp, "sparseMatrix")) {
    A_binary_mp <- Matrix::Matrix(A_binary_mp, sparse = TRUE)
  }

  return(A_binary_mp)
}
