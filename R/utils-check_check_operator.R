#' @title  Check operator validity
#'
#' @description
#'
#' Validates that the operator is one of '=', '<=', '>='.
#'
#' @keywords internal
#' @noRd
#'

check_operator <- function(operator) {
  valid_ops <- c("<=", "=", ">=")
  if (!all(operator %in% valid_ops)) {
    stop(paste0("Invalid operator: Must be one of ", paste(valid_ops, collapse = ", "), "."))
  }
  return(operator)
}
