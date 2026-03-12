#' @title Validate module selection for constraints
#'
#' @description
#' Ensures that the user-specified module indices are valid for the given mstATA object.
#' @noRd
#' @keywords internal
#'
#' @param which_module A vector of module indices, or NULL to select all.
#' @param num_modules An integer indicating the number of modules in a panel.
#'
#' @return A validated integer vector of module indices.
#'

validate_module_selection <- function(which_module, num_modules) {
  if (is.null(which_module)) {
    return(seq_len(num_modules))  # Use all modules if not specified
  }

  if (!is.numeric(which_module) || any(which_module %% 1 != 0)) {
    stop("'which_module' must be a vector of integers.")
  }

  if (any(which_module < 1 | which_module > num_modules)) {
    stop(paste0("Invalid 'which_module' index: must be between 1 and ", num_modules, "."))
  }

  return(sort(unique(which_module)))
}
