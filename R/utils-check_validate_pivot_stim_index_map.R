#' @title Validate Pivot–Stimulus Mapping (Index Representation)
#'
#' @description
#' Validates a pivot–stimulus mapping after item identifiers have been
#' converted to row indices. This function assumes that
#' \code{validate_pivot_stim_map()} has already been called on the ID-based
#' mapping and therefore checks only index-level invariants.
#'
#' @param pivot_stim_map A pivot–stimulus mapping object whose elements contain
#'   item row indices.
#' @param itempool A data frame representing the current item pool.
#'
#' @return \code{TRUE} if the index mapping is valid; otherwise an error is thrown.
#'
#' @keywords internal
#' @noRd
validate_pivot_stim_index_map <- function(pivot_stim_map, itempool) {
  pool_size<-nrow(itempool)

  confirm_index(
    pivot_stim_map$pivot_item_id,
    pool_size,
    what = "pivot_item_id"
  )

  confirm_index(
    pivot_stim_map$stimulus_members,
    pool_size,
    what = "stimulus_members"
  )

  invisible(TRUE)
}
