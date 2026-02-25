#' @title Print mstATA compiled panel model
#'
#' @description
#' Prints a concise, human-readable summary of a compiled
#' \code{mstATA_model} object produced by \code{onepanel_spec()} or
#' \code{multipanel_spec()}.
#'
#' This print method is a **diagnostic and inspection utility**. It reports:
#'
#' \itemize{
#'   \item the number of binary and continuous decision variables,
#'   \item the total number of linear constraints,
#'   \item the objective sense (\code{"min"} or \code{"max"}),
#'   \item and a formatted specification table mapping semantic requirements
#'         to solver constraint rows.
#' }
#'
#' The specification table is rendered using
#' \code{print_specification()}, preserving row-range traceability
#' for debugging infeasibility and constraint prioritization.
#'
#' @param x An object of class \code{"mstATA_model"}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return The input object \code{x}, returned invisibly.
#'
#' @seealso
#' \code{\link{onepanel_spec}},
#' \code{\link{multipanel_spec}},
#'
#' @export



print.mstATA_model <- function(x, ...) {

  cat("mstATA panel specification\n")
  cat(strrep("-", 27), "\n", sep = "")

  ## ---- Decision variables ----
  if (!is.null(x$vtype)) {
    n_bin  <- sum(x$vtype == "B")
    n_cont <- sum(x$vtype == "C")

    cat("Decision variables:\n")
    cat("  Binary     :", n_bin, "\n")
    cat("  Continuous :", n_cont, "\n\n")
  }

  ## ---- Constraints ----
  n_con <- max(
    if (!is.null(x$A_binary)) nrow(x$A_binary) else 0L,
    if (!is.null(x$A_real))   nrow(x$A_real)   else 0L
  )

  cat("Constraints:\n")
  cat("  Number of rows:", n_con, "\n\n")

  ## ---- Objective ----
  if (!is.null(x$sense)) {
    cat("Objective sense:", x$sense, "\n\n")
  }

  ## ---- Specification table ----
  if (!is.null(x$specification)) {
    cat("Specification summary:\n")
    print_specification(x$specification)
  } else {
    cat("No specification table available.\n")
  }


  invisible(x)
}
