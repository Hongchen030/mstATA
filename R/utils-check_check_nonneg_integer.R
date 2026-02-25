#' @keywords internal
#' @noRd
check_nonneg_integer <- function(input,input_name ="Input",tol = 1e-12) {
  if (!is.numeric(input)) {
    stop(sprintf("`%s` must be numeric.", input_name), call. = FALSE)
  }
  if (!all(is.finite(input))) {
    stop(sprintf("`%s` contains NA, NaN, or infinite values.", input_name),
         call. = FALSE)
  }
  if (any(input < 0)) {
    stop(sprintf("`%s` must contain only nonnegative values.", input_name),
         call. = FALSE)
  }
  if (any(abs(input - round(input)) > tol)) {
    stop(sprintf("`%s` must contain only integer values.", input_name),
         call. = FALSE)
  }
  invisible(TRUE)
}
