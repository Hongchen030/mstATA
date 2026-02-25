#' @keywords internal
#' @noRd
confirm_index <- function(index,pool_size,what = "index",allow_list = TRUE) {
  if (allow_list && is.list(index)) {
    lapply(index, confirm_index, pool_size = pool_size, what = what, allow_list = FALSE)
    return(TRUE)
  }

  if (!is.integer(index)) {
    stop(sprintf("%s must be an integer vector.", what), call. = FALSE)
  }

  if (anyNA(index)) {
    stop(sprintf("%s contains NA values.", what), call. = FALSE)
  }

  if (any(index < 1 | index > pool_size)) {
    stop(
      sprintf("%s contains values outside [1, %d].", what, pool_size),
      call. = FALSE
    )
  }

  TRUE
}

