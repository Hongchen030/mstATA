#' @keywords internal
#' @noRd
align_binary_to_universe <- function(A_binary, dv_universe) {

  if (!inherits(A_binary, "sparseMatrix")) {
    A_binary <- Matrix::Matrix(A_binary, sparse = TRUE)
  }


  if (identical(colnames(A_binary), dv_universe)) {
    return(A_binary)
  }

  missing <- setdiff(colnames(A_binary), dv_universe)
  if (length(missing) > 0L) {
    stop("A_binary contains variables not in dv_universe: ",
         paste(missing, collapse = ", "),
         call. = FALSE)
  }

  Ab <- Matrix::Matrix(
    0,
    nrow = nrow(A_binary),
    ncol = length(dv_universe),
    sparse = TRUE,
    dimnames = list(NULL, dv_universe)
  )

  Ab[, colnames(A_binary)] <- A_binary
  Ab
}
