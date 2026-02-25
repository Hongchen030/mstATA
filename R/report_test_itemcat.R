#' @title Summarize Categorical Item Attributes in Assembled MST Panels
#'
#' @description
#' Summarizes the number of selected items belonging to specified categorical
#' attribute levels (e.g., content area, item type) within assembled MST panels.
#' Summaries can be produced at the module level or pathway level, depending on
#' the constraint scope.
#'
#' @details
#' This function operates on the output of \code{assembled_panel()} and is
#' primarily intended for evaluating categorical test constraints.
#'
#' The scope of the summary is determined by \code{which_module} and
#' \code{which_pathway}:
#' \itemize{
#'   \item If \code{which_module} is provided, a module-level summary is returned
#'     for the specified modules.
#'   \item If \code{which_pathway} is provided, a pathway-level summary is returned
#'     for the specified pathways.
#'   \item If both \code{which_module} and \code{which_pathway} are \code{NULL},
#'     the function defaults to a \strong{module-level summary over all modules}.
#' }
#'
#' The attribute specified by \code{attribute} must be a categorical attribute.
#' This is verified internally using \code{check_attribute_column()}, and an
#' error is thrown if the attribute is not categorical.
#'
#' Counts are reported \emph{separately for each requested attribute level}.
#' For example, if \code{cat_levels = c("MC", "TEI")}, the output will
#' contain separate rows for MC and TEI items within each module or pathway.
#'
#' @param assembled_panel
#' A \code{"mstATA_panel"} object returned by
#' \code{\link{assembled_panel}}. All panels must share identical module and
#' pathway structure.
#' @param attribute A character string specifying the name of a categorical
#'   attribute in the item pool (e.g., \code{"content"}, \code{"itemtype"}).
#' @param cat_levels A character vector specifying the attribute levels
#'   to be summarized (e.g., \code{"MC"} or \code{c("MC", "TEI")}).
#' @param which_module Optional integer vector specifying module indices for
#'   module-level summaries.
#' @param which_pathway Optional integer vector specifying pathway indices for
#'   pathway-level summaries.
#'
#' @return A data frame with one row per panel–module (or panel–pathway)
#'   combination. The returned data frame contains:
#' \describe{
#'   \item{panel_id}{Panel identifier.}
#'   \item{module_id or pathway_id}{Module or pathway identifier.}
#'   \item{cat_level_1, cat_level_2, \ldots}{Number of selected items belonging
#'     to each specified categorical level within the corresponding module
#'     or pathway. Each category level appears as a separate column.}
#' }
#'
#' @seealso [assembled_panel()]
#'
#' @export

report_test_itemcat <- function(assembled_panel,
                                attribute,cat_levels,
                                which_module = NULL,which_pathway = NULL) {
  if (!inherits(assembled_panel, "mstATA_panel")) {
    stop("Input 'assembled_panel' must be an object of class 'mstATA_panel'.",
         call. = FALSE)
  }

  if (!is.character(attribute) || length(attribute) != 1L) {
    stop("'attribute' must be a single character string.", call. = FALSE)
  }
  if (!is.character(cat_levels) || length(cat_levels) == 0L) {
    stop("'cat_levels' must be a non-empty character vector.", call. = FALSE)
  }
  first_panel <- assembled_panel[[1]]
  NumModules  <- length(unique(first_panel$ItemsInModules$module_id))
  NumPathways <- length(unique(first_panel$ItemsInPathways$pathway_id))

  check_scope<-check_test_constraint_scope(num_modules = NumModules,num_pathways = NumPathways,
                                           which_module = which_module, which_pathway = which_pathway)
  scope_level<-check_scope[["application_level"]]
  which_module <- check_scope[["which_module"]]
  which_pathway <- check_scope[["which_pathway"]]


  out <- lapply(names(assembled_panel), function(panel_name) {

    panel <- assembled_panel[[panel_name]]

    ## ---- select data source ----
    dat <- switch(
      scope_level,
      "Module-level"  = panel$ItemsInModules,
      "Pathway-level" = panel$ItemsInPathways
    )

    attr_checked <- check_attribute_column(dat, attribute)
    if (attr(attr_checked, "attribute_type") != "categorical") {
      stop("Attribute '", attribute, "' is not a categorical attribute.",
           call. = FALSE)
    }

    ## ---- subset by scope ----
    if (scope_level == "Module-level") {
      dat <- dat[dat$module_id %in% which_module, , drop = FALSE]
      id_col  <- "module_id"
      id_vals <- sort(unique(dat$module_id))
    } else {
      dat <- dat[dat$pathway_id %in% which_pathway, , drop = FALSE]
      id_col  <- "pathway_id"
      id_vals <- sort(unique(dat$pathway_id))
    }

    dat[[id_col]] <- factor(dat[[id_col]], levels = id_vals)
    dat[[attribute]] <- factor(dat[[attribute]], levels = cat_levels)

    ## ---- vectorized counting ----
    count_mat <- table(dat[[id_col]], dat[[attribute]])

    res <- data.frame(
      panel_id = panel_name,
      id = id_vals,
      as.data.frame.matrix(count_mat),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    names(res)[2] <- id_col

    ## ---- relabel IDs if index columns exist ----
    if (scope_level == "Module-level" && "module_index" %in% names(dat)) {
      map <- unique(dat[, c("module_id", "module_index")])
      map <- map[order(map$module_id), ]
      res[[id_col]] <- factor(res[[id_col]],levels = map$module_id,
                              labels = map$module_index)
    }

    if (scope_level == "Pathway-level" && "pathway_index" %in% names(dat)) {
      map <- unique(dat[, c("pathway_id", "pathway_index")])
      map <- map[order(map$pathway_id), ]
      res[[id_col]] <- factor(res[[id_col]],levels = map$pathway_id,
                              labels = map$pathway_index)
    }

    res
  })

  do.call(rbind,out)
}
