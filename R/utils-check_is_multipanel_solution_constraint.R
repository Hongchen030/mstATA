#' Identify multi-panel solution-level constraints
#'
#' Determines whether a solution-level constraint is defined over multiple panels
#' and therefore should not be included in a single-panel specification.
#'
#' @param constraint An \code{mstATA_constraint} object.
#' @param x An object of class `"mstATA_design"` created by `mst_design()`.
#'
#' @return Logical scalar indicating whether the constraint is multi-panel.
#'
#' @details
#' A solution-level constraint is considered multi-panel if the number of columns
#' in its binary constraint matrix exceeds the sum of single-panel decision
#' variables and solution-level item indicators. Such constraints must be applied
#' during multi-panel expansion rather than in \code{onepanel_spec()}.
#'
#' @keywords internal
#' @noRd


is_multipanel_solution_constraint <- function(constraint,x) {

  if (!inherits(constraint, "mstATA_constraint")) {
    stop("Internal error: expected an mstATA_constraint.", call. = FALSE)
  }

  if (!inherits(x, "mstATA_design")) {
    stop("Input 'x' must be an object of class 'mstATA_design'.",call. = FALSE)
  }

  if (!any(constraint$specification$`Application Level` == "Solution-level",
           na.rm = TRUE)) {
    return(FALSE)
  }


  if (is.null(constraint$A_binary)) {
    return(FALSE)
  }

  PoolSize<-nrow(x$ItemPool)
  return(ncol(constraint$A_binary) == PoolSize)
}

