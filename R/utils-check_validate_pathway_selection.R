#' @title Validate pathway selection for constraints
#' @description
#'
#' Ensures the provided pathway indices are valid. If NULL, returns all pathways.
#'
#' @keywords internal
#' @noRd
#'
#' @param which_pathway A numeric vector of pathway indices (e.g., c(1, 2, 5)) or NULL to use all pathways.
#' @param num_pathways An integer indicating the number of allowable pathways in a MST panel.
#'
#' @return A sorted unique vector of valid pathway indices.
#'

validate_pathway_selection <- function(which_pathway, num_pathways) {
  if (is.null(which_pathway)) {
    return(seq_len(num_pathways))  # Use all pathways
  }

  if (!is.numeric(which_pathway) || any(which_pathway %% 1 != 0)) {
    stop("'which_pathway' must be a vector of integers.")
  }

  if (any(which_pathway < 1 | which_pathway > num_pathways)) {
    stop(paste0("Invalid 'which_pathway' index: must be between 1 and ", num_pathways, "."))
  }

  return(sort(unique(which_pathway)))
}
