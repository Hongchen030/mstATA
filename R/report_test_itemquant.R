#' @title Summarize Quantitative Item Attributes in Assembled MST Panels
#'
#' @description
#' Summarizes quantitative item attributes (e.g., test information, time,
#' difficulty) within assembled multistage test (MST) panels. The summary can
#' be computed as either the sum or the average of the attribute values, at
#' the module level or pathway level, depending on the constraint scope.
#'
#' @details
#' This function operates on the output of \code{assembled_panel()} and is
#' primarily intended for evaluating quantitative test constraints.
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
#' The attribute specified by \code{attribute} must be a quantitative attribute.
#' This is verified internally using \code{check_attribute_column()}, and an
#' error is thrown if the attribute is not quantitative.
#'
#' @param assembled_panel
#' A \code{"mstATA_panel"} object returned by
#' \code{\link{assembled_panel}}. All panels must share identical module and
#' pathway structure.
#' @param attribute A character string specifying the name of a
#'   \strong{quantitative} attribute in the item pool (e.g., \code{"time"},
#'   \code{"difficulty"}, \code{"information"}).
#' @param statistic A character string specifying the summary statistic to
#'   compute. One of \code{"average"} or \code{"sum"}.
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
#'   \item{attribute}{The name of the quantitative attribute being summarized.}
#'   \item{sum or average}{The computed summary statistic (sum or average) of the
#'     specified quantitative attribute within the corresponding module
#'     or pathway.}
#' }
#'
#' @seealso [assembled_panel()]
#'
#' @export


report_test_itemquant <- function(assembled_panel,attribute, statistic = c("average","sum"),
                                  which_module = NULL,which_pathway = NULL) {
  if (!inherits(assembled_panel, "mstATA_panel")) {
    stop("Input 'assembled_panel' must be an object of class 'mstATA_panel'.",
         call. = FALSE)
  }

  if (!is.character(attribute) || length(attribute) != 1L) {
    stop("'attribute' must be a single character string.", call. = FALSE)
  }
  statistic<-match.arg(statistic,choices = c("average","sum"))
  fun <- if (statistic == "sum") sum else mean

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
    if (attr(attr_checked, "attribute_type") != "quantitative") {
      stop("Attribute '", attribute, "' is not a quantitative attribute.",
           call. = FALSE)
    }

    ## ---- module-level summary ----
    if (scope_level =="Module-level") {
      dat <- dat[dat$module_id %in% which_module, , drop = FALSE]
      if (nrow(dat) == 0) return(NULL)
      res <- aggregate(
        dat[[attribute]],
        by = list(module_id = dat$module_id),
        FUN = fun
      )

      colnames(res)[ncol(res)] <- statistic
      res$attribute <- attribute
      res$panel_id<-panel_name
      res <- res[, c("panel_id","module_id", "attribute", statistic), drop = FALSE]
    }

    ## ---- pathway-level summary ----
    if (!is.null(which_pathway)) {
      dat <- dat[dat$pathway_id %in% which_pathway, , drop = FALSE]
      if (nrow(dat) == 0) return(NULL)
      res <- aggregate(
        dat[[attribute]],
        by = list(pathway_id = dat$pathway_id),
        FUN = fun
      )

      colnames(res)[ncol(res)] <- statistic
      res$attribute <- attribute
      res$panel_id<-panel_name
      res <- res[, c("panel_id","pathway_id","attribute", statistic), drop = FALSE]
    }

    if(scope_level =="Module-level" && "module_index"%in%names(dat)){
      map <- unique(dat[, c("module_id", "module_index")])
      map <- map[order(map$module_id), ]
      res$module_id<-factor(res$module_id,levels = dat$module_id,
                            labels = dat$module_index)
    }
    if(scope_level =="Pathway-level" && "pathway_index"%in%names(dat)){
      map <- unique(dat[, c("pathway_id", "pathway_index")])
      map <- map[order(map$pathway_id), ]
      res$pathway_id<-factor(res$pathway_id,levels = dat$pathway_id,
                             labels = dat$pathway_index)
    }
    return(res)
  })

  do.call(rbind,out)
}

