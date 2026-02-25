#' @title check the min/max
#'
#' @keywords internal
#' @noRd
#'
validate_min_max <- function(min, max) {
  if (!is.null(min) && !is.null(max) && min > max) {
    stop("'min' cannot be greater than 'max'.")
  }
  if (!is.null(min)) {
    if (!is.numeric(min) || length(min) != 1L)
      stop("'min' must be a numeric scalar.")
  }

  if (!is.null(max)) {
    if (!is.numeric(max) || length(max) != 1L)
      stop("'max' must be a numeric scalar.")
  }
  return(invisible(TRUE))

}
