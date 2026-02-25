#' @title Validate and expand min/max or target/deviation bounds
#'
#' @description
#'
#' Ensures that target ± deviation or min/max inputs are expanded to consistent matrices.
#'
#' @param levels A character vector of attribute categories (for categorical) or a single attribute name (for quantitative).
#' @param min Minimum values (scalar, vector, or matrix).
#' @param max Maximum values (scalar, vector, or matrix).
#' @param target Target values (scalar, vector, or matrix).
#' @param deviation Allowable deviations (scalar, vector, or matrix).
#' @param total_rows A scalar indicating either the total number of pathways or the total number of modules in the MST.
#' @param row_ids A vector of selected modules or pathways (e.g., \code{which_module} or \code{which_pathway}).
#' If \code{NULL}, all modules are used.
#'
#' @return A list with two components: `min` and `max`, both as matrices.
#' @keywords internal
#' @noRd

check_target_bounds <- function(levels,min = NULL, max = NULL,
                                target = NULL, deviation = NULL,
                                total_rows,row_ids) {

  if (!is.null(target)) {
    if(is.null(deviation)){
      stop("You must either provide 'min' and 'max', or 'target' and 'deviation'.")
    }
    target_mat <- expand_target_to_matrix(input = target, levels = levels,
                                          total_rows = total_rows, row_ids = row_ids)
    deviation_mat <- expand_target_to_matrix(input = deviation,levels = levels,
                                             total_rows = total_rows,row_ids = row_ids)
    min<-target_mat-deviation_mat
    max<-target_mat+deviation_mat
  }else{
    if (is.null(min) || is.null(max)) {
      stop("You must either provide 'min' and 'max', or 'target' and 'deviation'.")
    }
    if(!is.matrix(min)){
      min <- expand_target_to_matrix(input = min,levels = levels,
                                     total_rows = total_rows,row_ids = row_ids)
    }
    if(!is.matrix(max)){
      max <- expand_target_to_matrix(input = max,levels = levels,
                                     total_rows = total_rows,row_ids = row_ids)
    }
  }

  # Identify violations (TRUE where max < min)
  violation <- (max < min)
  if (any(violation)) {
    stop("Invalid bounds: some 'max' values are smaller than 'min'.")
  }
  return(list(min=min,max=max))
}
