#' @title Expand item decision variable indices across multiple panels
#'
#' @description
#' Given the base item–module decision variable indices (x_im),
#' expand them across \code{num_panels} to obtain the column indices
#' of the item–module–panel variables (x_imp).
#'
#' @param x An \code{mstATA_design} object containing \code{decisionvar_name}.
#' @param num_panels Integer scalar; number of panels.
#'
#' @return
#' A list of length \code{PoolSize}.
#' Entry \code{[[i]]} contains ALL column indices of x_imp variables
#' for item \eqn{i} across all modules × all panels.
#'
#' @keywords internal
#' @noRd
#'
expand_itemdv_panels <- function(x,num_panels) {

  if (!inherits(x, "mstATA_design")){
    stop("x must be an object of class 'mstATA_design'.")
  }

  PoolSize <- nrow(x$ItemPool)
  # base item-module dv
  item_x_cols_base <- check_itemdv(x)

  # size of x_im block
  block_size <- length(x$decisionvar_name)

  item_x_cols <- vector("list", PoolSize)

  for (i in seq_len(PoolSize)) {

    cols <- integer()

    for (panel_id in seq_len(num_panels)) {

      shift <- (panel_id - 1) * block_size

      cols <- c(cols, item_x_cols_base[[i]] + shift)
    }

    item_x_cols[[i]] <- cols
  }

  item_x_cols
}
