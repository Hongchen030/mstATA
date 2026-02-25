#' @title Detect need for item–stimulus linking
#'
#' @description
#'
#' Determines whether item–stimulus gating constraints are required for a panel
#' based on constraint specifications. Item–stimulus linking is needed when at
#' least one constraint is a logical requirement applied at the module level.
#'
#' @param constraints A list of \code{mstATA_constraint} objects.
#'
#' @return Logical scalar indicating whether \code{dvlink_item_stim} should be added.
#'
#' @details
#' Item–stimulus linking is triggered when a constraint has
#' \code{Type == "Logical"} and \code{Application Level == "Module-level"} in its
#' specification table.
#'
#' @noRd
#' @keywords internal
#' @noRd
needs_dvlink_item_stim <- function(constraints) {

  if (!is.list(constraints) ||
      !all(vapply(constraints, inherits, logical(1), "mstATA_constraint"))) {
    stop("`constraints` must be a list of 'mstATA_constraint' objects.",
         call. = FALSE)
  }


  has_module_logical <- vapply(
    constraints,
    function(con) {
      spec <- con$specification
      any(
        spec$Type == "Logical" &
          spec$`Application Level` == "Module-level",
        na.rm = TRUE
      )
    },
    logical(1)
  )

  has_itemstim_gating<-vapply(
    constraints,
    function(con) {
      spec <- con$specification
      any(spec$Type == "Logical" &
          spec$`Application Level` == "Module-level" &
          grepl("Within-stimulus item count( from)?", spec$Requirement),
          na.rm = TRUE)
    },
    logical(1)
  )
  return(any(has_module_logical) && !any(has_itemstim_gating))
}
