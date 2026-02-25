#' @title Validate Pivot–Stimulus Mapping
#'
#' @description
#' Validates a pivot–stimulus mapping created by
#' \code{create_pivot_stimulus_map()}. This validation operates on
#' **item identifiers**, not row indices, and is intended for use at the
#' MST design stage.
#'
#' @param pivot_stim_map A list describing the pivot–stimulus mapping.
#' @param item_id_col A string giving the column name in \code{itempool}
#'   that uniquely identifies items.
#'
#' @details
#' The function performs the following validations:
#' \itemize{
#'   \item Required fields exist and share consistent lengths.
#'   \item \code{pivot_item_id} and \code{stimulus_name} contain no duplicates.
#'   \item \code{numItems_stimulus} matches the observed number of members in
#'     each stimulus.
#'   \item Each pivot item appears in its corresponding stimulus member set.
#'   \item All referenced item IDs must be valid.
#'   \item Items may not appear in more than one stimulus (disjointness check).
#' }
#'
#' @return
#' Returns \code{TRUE} invisibly if all checks pass. The function throws an
#' informative error if the mapping is invalid.
#'
#' @keywords internal

validate_pivot_stim_map <- function(pivot_stim_map, itempool,item_id_col) {
  if (!is.list(pivot_stim_map)) {
    stop("`pivot_stim_map` must be a list.", call. = FALSE)
  }
   required_fields <- c("pivot_item_id","stimulus_name",
                        "stimulus_members","numItems_stimulus")
   missing_names <- setdiff(required_fields,names(pivot_stim_map))
   if (length(missing_names) > 0) {
     stop(paste0("Invalid pivot_stim_map: must contain fields: ",paste(missing_names, collapse = ", ")),
          call. = FALSE)
   }

  pivot_item_id     <- pivot_stim_map$pivot_item_id
  stimulus_name     <- pivot_stim_map$stimulus_name
  stimulus_members  <- pivot_stim_map$stimulus_members
  numItems_stimulus <- pivot_stim_map$numItems_stimulus

  # Check lengths are consistent
  n <- length(pivot_item_id)

  if (!(length(stimulus_name)     == n &&
        length(stimulus_members)  == n &&
        length(numItems_stimulus) == n)) {
    stop("Invalid pivot_stim_map: lengths of all components must match.",
         call. = FALSE)
  }

  # pivot_item must be unique
  if(anyDuplicated(pivot_item_id)){
    stop("Invalid pivot_stim_map: pivot_item_id contains duplicates.")
  }
  # stimulus_name must be unique
  if (anyDuplicated(stimulus_name)) {
    stop("Invalid pivot_stim_map: stimulus_name contains duplicates.",
         call. = FALSE)
  }

  if (any(vapply(stimulus_members, length, integer(1)) == 0)) {
    stop("Invalid pivot_stim_map: stimulus_members cannot be empty.",
         call. = FALSE)
  }

  # Cross-check membership counts
  observed_counts <- vapply(stimulus_members, length, integer(1L))
  if (!all(observed_counts == numItems_stimulus)) {
    stop("Invalid pivot_stim_map: numItems_stimulus does not match stimulus_members.",
         call. = FALSE)
  }

  # Every pivot must be a member of its own stimulus group
  for (i in seq_len(n)) {
    if (!(pivot_item_id[i] %in% stimulus_members[[i]])) {
      stop(sprintf(
        "Invalid pivot_stim_map: pivot item '%s' is not included in its stimulus '%s'.",
        pivot_item_id[i], stimulus_name[i]),call. = FALSE)
    }
  }

  # Cross-stimulus disjointness
  all_members <- unlist(stimulus_members, use.names = FALSE)
  if (anyDuplicated(all_members)) {
    stop("Invalid pivot_stim_map: items cannot belong to multiple stimuli.",
         call. = FALSE)
  }

  item_ids<-as.character(check_attribute_column(itempool = itempool,attribute = item_id_col))
  missing_items <- setdiff(all_members, item_ids)
  if (length(missing_items) > 0) {
    stop(paste0("Invalid pivot_stim_map: item IDs not found in itempool: ",
                paste(missing_items, collapse = ", ")),call. = FALSE)
  }

  invisible(TRUE)
}
