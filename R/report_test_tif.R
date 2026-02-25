#' @title Report Test Information Functions (TIF)
#'
#' @description
#'
#' Computes and reports test information functions aggregated at the
#' module or pathway level for assembled MST panels.
#'
#' Item information functions are computed using \code{compute_iif()},
#' and then summed across items within specified module or pathway.
#'
#' @param assembled_panel
#' A \code{"mstATA_panel"} object returned by
#' \code{\link{assembled_panel}}. All panels must share identical module and
#' pathway structure.
#' @param theta A numeric vector of ability values at which information
#'   functions are evaluated.
#' @param item_par_cols A named list defining the IRT parameter columns
#'   required for each supported model. The list names must exactly match
#'   the model identifiers specified in \code{items[[model_col]]}:
#'   \code{"1PL"}, \code{"RASCH"}, \code{"2PL"}, \code{"3PL"},
#'   \code{"4PL"}, \code{"GRM"}, \code{"MGRM"}, \code{"PCM"},
#'   \code{"GPCM"}, \code{"RSM"}, and \code{"NRM"}.
#'   Each list element is a character vector giving the column names
#'   that contain the required item parameters for
#'   the corresponding model.
#' @param model_col A character string specifying the column name in
#'   either \code{ItemsInModules} or \code{ItemsInPathways} data frames
#'   that indicates the IRT model used for each item.
#'   Values in this column must correspond to one of the supported
#'   model names:
#'   \code{"1PL"}, \code{"RASCH"}, \code{"2PL"}, \code{"3PL"},
#'   \code{"4PL"}, \code{"GRM"}, \code{"MGRM"}, \code{"PCM"},
#'   \code{"GPCM"}, \code{"RSM"}, or \code{"NRM"}.
#' @param D Scaling constant used in the IRT model. Default is D=1 (for logistic metric);
#'  D=1.702 yields approximately the normal metric.
#' @param which_module Optional integer vector specifying which modules
#'   to include.
#' @param which_pathway Optional integer vector specifying which pathways
#'   to include.
#'
#' @details
#' For polytomous IRT models, \code{report_test_tif()} assumes that
#' item parameters are supplied in the exact order and structure required
#' by the underlying \code{compute_icc()} and \code{compute_iif()} function.
#' Details about the item parameters can see helper function \code{Pi_internal()}.
#'
#' @return A data frame with one row per panel–module (or panel–pathway)–ability
#'   combination. The returned data frame contains:
#' \describe{
#'   \item{panel_id}{Panel identifier.}
#'   \item{module_id or pathway_id}{Module or pathway identifier.}
#'   \item{theta}{Ability value at which test information is evaluated.}
#'   \item{information}{Sum of item information functions for all selected items
#'     within the corresponding module or pathway, evaluated at the given
#'     ability value.}
#' }
#'
#' @seealso [Pi_internal()], [compute_icc()], [compute_iif()]
#'
#' @export


report_test_tif<-function(assembled_panel,theta,
                          item_par_cols,model_col, D = 1,
                          which_module = NULL, which_pathway = NULL){
  ## ---- require validated assembled panel ----
  if (!inherits(assembled_panel, "mstATA_panel")) {
    stop("Input 'assembled_panel' must be an object of class 'mstATA_panel'.",
         call. = FALSE)
  }

  if (!is.numeric(theta) || length(theta) == 0L) {
    stop("'theta' must be a non-empty numeric vector.", call. = FALSE)
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
    dat <- if (scope_level == "Module-level") {
      panel$ItemsInModules
    } else {
      panel$ItemsInPathways
    }

    iif_matrix<-compute_iif(items = dat,item_par_cols = item_par_cols,theta = theta,
                            model_col = model_col,D= D)

    group_var <- if (scope_level == "Module-level") {
      "module_id"
    } else {
      "pathway_id"
    }
    split_idx <- split(seq_len(nrow(dat)), dat[[group_var]])
    group_ids <- if (scope_level == "Module-level") {
      which_module
    } else {
      which_pathway
    }

    res <- lapply(group_ids, function(g) {

      idx <- split_idx[[as.character(g)]]

      info <- if (!is.null(idx)) {
        colSums(iif_matrix[idx, , drop = FALSE])
      } else {
        rep(0, length(theta))
      }

      out_df <- data.frame(
        panel_id = panel_name,
        theta = theta,
        information = info,
        row.names = NULL
      )

      if (scope_level == "Module-level") {
        out_df$module_id <- g
      } else {
        out_df$pathway_id <- g
      }

      out_df
    })

    panel_result <- do.call(rbind, res)

    if(scope_level =="Module-level" && "module_index"%in%names(dat)){
      map <- unique(dat[, c("module_id", "module_index")])
      panel_result$module_id<-factor(panel_result$module_id,levels = map$module_id,
                                     labels = map$module_index)
    }
    if(scope_level =="Pathway-level" && "pathway_index"%in%names(dat)){
      map <- unique(dat[, c("pathway_id", "pathway_index")])
      panel_result$pathway_id<-factor(panel_result$pathway_id,levels = map$pathway_id,
                                      labels = map$pathway_index)
    }
    panel_result
  })

  do.call(rbind,out)
}


