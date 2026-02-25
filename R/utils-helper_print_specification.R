#' @title Print a formatted specification table for a panel specification
#'
#' @description
#'
#' This function is a **low-level formatting utility** intended for inspecting
#' specification tables extracted from \code{"mstATA_panel_spec"} objects.
#'
#' It reorders columns, optionally adds a human-readable row-range column
#' (e.g., `"rows 15–28"`), and prints the result for interactive inspection.
#' The function is strictly *non-mutating*: it does not modify global options,
#' solver models, or the underlying panel specification object.
#'
#'
#' @details
#'
#' The printed specification table provides a semantic summary of how
#' high-level requirements (constraints, objectives, and automatically injected
#' decision-variable linking rules) map onto contiguous row ranges in the
#' compiled constraint matrix.
#'
#' This view is particularly useful for:
#'
#' \itemize{
#'   \item diagnosing infeasibility by identifying conflicting constraint blocks;
#'   \item understanding which rows correspond to auto-injected constraints
#'         (e.g., decision-variable linking);
#'   \item prioritizing or relaxing groups of constraints programmatically; and
#'   \item debugging solver output with respect to original test specifications.
#' }
#'
#' @keywords internal

print_specification <- function(spec) {
  if (!is.data.frame(spec)) {
    stop("`spec` must be a specification data.frame.", call. = FALSE)
  }

  df <- spec

  if (all(c("Row_Start", "Row_End") %in% names(df))) {
    df$Rows <- ifelse(
      is.na(df$Row_Start) | is.na(df$Row_End),
      NA_character_,
      ifelse(
        df$Row_Start == df$Row_End,
        as.character(df$Row_Start),
        paste0(df$Row_Start, "-", df$Row_End)
      )
    )
  }

  if ("Type" %in% names(df)) {
    df$Type <- factor(
      df$Type,
      levels = c("Logical", "Categorical", "Quantitative")
    )
  }

  if ("Application Level" %in% names(df)) {
    df$`Application Level` <- factor(
      df$`Application Level`,
      levels = c(
        "Module-level",
        "Pathway-level",
        "Panel-level",
        "Solution-level"
      )
    )
  }

  if (all(c("Type", "Application Level") %in% names(df))) {
    df <- df[order(df$Type, df$`Application Level`), , drop = FALSE]
  }

  preferred <- c("Source","Requirement","Attribute",
                 "Type","Application Level","Operator","Num of Constraints","Panel","Rows")

  keep <- intersect(preferred, names(df))
  df <- df[, keep, drop = FALSE]
  if("Panel" %in% names(df)){
    df<-df[order(df$Panel),]
  }
  rownames(df) <- NULL
  print(df, right = FALSE)
  invisible(df)
}
