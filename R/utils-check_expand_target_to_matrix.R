#' @title Expand scalar/vector/matrix to target matrix
#' @description
#'
#' Converts a scalar, vector, or matrix into a matrix of shape (n_rows x n_columns),
#' This function supports both categorical attributes (with multiple levels) and
#' quantitative attributes (single numeric feature).
#'
#' @param input A scalar, vector, or matrix to be expanded.
#' @param levels A character vector of attribute categories (for categorical) or a single attribute name (for quantitative).
#' @param total_rows A scalar indicating either the total number of pathways or the total number of modules in the MST.
#' @param row_ids A vector of selected modules or pathways (e.g., \code{which_module} or \code{which_pathway}).
#' @return A numeric matrix of dimension \code{(n_rows x n_columns)}.
#'
#' @keywords internal
#' @noRd

expand_target_to_matrix <- function(input,levels,total_rows,row_ids) {
  n_rows <- length(row_ids)
  n_cols <- if (is.null(levels) | length(levels) == 1L) 1 else length(levels)

  if (is.matrix(input)) {
    input_dim<-dim(input)
    if (all(input_dim == c(total_rows, n_cols))) {
      input <- input[row_ids, , drop = FALSE]
      return(input)
    }
    if (all(input_dim == c(n_rows, n_cols))) {
      return(input)
    }

    stop(paste0("Input matrix must have dimension (", n_rows, " x ", n_cols,
                ") or the full size (", total_rows, " x ", n_cols, ")."))
  }


  if (length(input) == 1L) {
    return(matrix(input, nrow = n_rows, ncol = n_cols))
  }

  if (length(input) == n_cols) {
    return(matrix(rep(input, each = n_rows), nrow = n_rows, ncol = n_cols, byrow = FALSE))
  }

  if (length(input) == n_rows){
    return(matrix(rep(input, each = n_cols), nrow = n_rows, ncol = n_cols,byrow = TRUE))
  }

  if (length(input) == total_rows && n_cols == 1L) {
    return(matrix(input[row_ids], nrow = n_rows, ncol = 1))
  }

  stop("Input cannot be expanded into the expected (n_rows x n_cols) matrix.")
}
