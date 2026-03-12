#' @keywords internal
#' @noRd
#'

add_row_ranges_from_spec <- function(spec, base_row) {
  counts <- spec$`Num of Constraints`
  start <- base_row + cumsum(c(1L, head(counts, -1L)))
  end   <- base_row + cumsum(counts)
  cbind(spec, Row_Start = start, Row_End = end)
}
