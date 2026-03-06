#' @title Report Test Category Probability Curves
#'
#' @description
#'
#' Computes and reports category probability curves aggregated at the
#' module or pathway level for assembled MST panels.
#'
#' Item-level category probabilities are obtained from
#' \code{compute_icc()} and averaged across items within each module or
#' pathway.
#'
#' @param assembled_panel
#' A \code{"mstATA_panel"} object returned by
#' \code{\link{assembled_panel}}. All panels must share identical module and
#' pathway structure.
#' @param theta A numeric vector of ability values.
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
#' @param which_module Optional vector specifying which modules to include.
#' @param which_pathway Optional vector specifying which pathways to include.
#'
#' @details
#' For polytomous IRT models, category probabilities are averaged across
#' items within each module or pathway. The function does not validate
#' the number or structure of item parameters; users are responsible for
#' supplying parameters compatible with \code{mstR::Pi()}.
#'
#' @return A named list of data frames, one per panel. Each data frame
#'   contains category probability curves with columns \code{cat0},
#'   \code{cat1}, \ldots.
#'
#' @seealso [compute_icc()]
#'
#' @export

report_test_tcc<-function(assembled_panel,theta,
                          item_par_cols,model_col,D = 1,
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
    dat <- switch(
      scope_level,
      "Module-level"  = panel$ItemsInModules,
      "Pathway-level" = panel$ItemsInPathways
    )

    icc_list<-compute_icc(dat,item_par_cols,theta,model_col = model_col,nrCat_col = NULL,
                          D = D)
    compute_tcc_subset <- function(idx) {
      tcc<-numeric(length(theta))
      icc_example <- icc_list[[1]][idx, , drop = FALSE]

      max_score_vec <- apply(icc_example, 1, function(item_probs) {
        nonzero <- which(item_probs > 1e-10)
        if (length(nonzero) == 0) return(0)
        max(nonzero) - 1
      })
      max_score <- sum(max_score_vec)

      # ---- Compute TCC ----
      for (t in seq_along(theta)) {

        icc_mat <- icc_list[[t]][idx, , drop = FALSE]

        score_vec <- 0:(ncol(icc_mat) - 1)

        tcc[t] <- sum(icc_mat %*% score_vec)
      }

      list(
        TCC = tcc,
        max_score = max_score,
        TCC_norm = tcc / max_score
      )
    }

    if (scope_level == "Module-level") {

      res <- lapply(which_module, function(m) {
        idx <- dat$module_id == m
        tcc_list <- compute_tcc_subset(idx)

        data.frame(
          panel_id  = panel_name,
          module_id = m,
          theta     = theta,
          TCC = tcc_list[["TCC"]],
          max_score = tcc_list[["max_score"]],
          TCC_norm = tcc_list[["TCC_norm"]],
          row.names = NULL
        )
      })

    } else {

      res <- lapply(which_pathway, function(p) {
        idx <- dat$pathway_id == p
        tcc_list <- compute_tcc_subset(idx)

        data.frame(
          panel_id   = panel_name,
          pathway_id = p,
          theta      = theta,
          TCC = tcc_list[["TCC"]],
          max_score = tcc_list[["max_score"]],
          TCC_norm = tcc_list[["TCC_norm"]],
          row.names = NULL
        )
      })
    }

    panel_result <- do.call(rbind, res)

    if(scope_level =="Module-level" && "module_index"%in%names(dat)){
      map <- unique(dat[, c("module_id", "module_index")])
      map <- map[order(map$module_id), ]
      panel_result$module_id<-factor(panel_result$module_id,levels = map$module_id,
                                     labels = map$module_index)
    }
    if(scope_level =="Pathway-level" && "pathway_index"%in%names(dat)){
      map <- unique(dat[, c("pathway_id", "pathway_index")])
      map <- map[order(map$pathway_id), ]
      panel_result$pathway_id<-factor(panel_result$pathway_id,levels = map$pathway_id,
                                      labels = map$pathway_index)
    }
    return(panel_result)
  })

  do.call(rbind,out)
}

