#' @title Build Item–Module Column Mapping for x_im Decision Variables
#'
#' @description
#'
#' Construct a list of column indices for each item’s decision variables
#' across all modules in a panel.
#'
#' @details
#' This function operates on the naming convention used in
#' \code{x$decisionvar_name}, where item–module variables are encoded as:
#'
#' \preformatted{
#'  "module <m> item <i>"
#' }
#'
#' In the `mstATA_design` multipanel framework, the base decision variables
#' \code{x_{m,s}} (item–module selection variables) are replicated for each
#' assembled panel to create panel-specific variables \code{x_{m,i,p}}.
#'
#' @param x An object of class `"mstATA_design"` created by `mst_design()`.
#' @return
#' A list of length \code{PoolSize}.
#' Entry \code{[[i]]} is an integer vector giving the column indices of all
#' decision variables \code{x_{m,i,p}} corresponding to item \eqn{i}, across all
#' modules in a panel.
#' @keywords internal
#' @noRd
check_itemdv <- function(x) {
  if (!inherits(x, "mstATA_design")){
    stop("x must be an object of class 'mstATA_design'.")
  }

  decnames <- x$decisionvar_name
  PoolSize <- nrow(x$ItemPool)
  NumModules <- x$NumModules

  item_x_cols <- vector("list", PoolSize)

  for (i in seq_len(PoolSize)) {
    patt <- paste0("^x\\[[0-9]+,", i, "\\]$")
    cols <- grep(patt, decnames)
    item_x_cols[[i]] <- cols
  }

  return(item_x_cols)
}
