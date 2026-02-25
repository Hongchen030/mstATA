#' @title Evaluate MST designs via recursive score-distribution propagation
#'
#' @description
#'
#' Convenience wrapper for \code{\link{analytic_mst_precision}} that constructs
#' module-level item category response probabilities (ICCs) directly from item
#' parameters before performing analytic MST precision evaluation.
#'
#' This function is intended for users who work with item pools and prefer a
#' single high-level entry point. Internally, it builds ICC objects on a fixed
#' ability grid and then calls the analytic core.
#'
#' @param design A character string specifying the MST design. The string encodes
#'   the number of modules per stage and may use commas (\code{,}), dashes
#'   (\code{-}), or slashes (\code{/}) as separators.
#'   For example, \code{"1-3-3"}, \code{"1,3,3"}, and \code{"1/3/3"} all define
#'   an MST with 1 module in stage 1, 3 modules in stage 2, and 3 modules in stage 3.
#' @param assembled_panel A \code{"mstATA_panel"} object returned by
#' \code{\link{assembled_panel}}. All panels share identical module and
#' pathway structure.
#' @param item_par_cols A named list defining the IRT parameter columns
#'   required for each supported model. The list names must exactly match
#'   the model identifiers specified in \code{model_col} column:
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
#' @param theta Numeric vector specifying the ability grid used to construct
#' ICCs. Defaults to \code{seq(-3, 3, 0.1)}.
#' @param range_tcc Numeric length-2 vector giving the search interval for
#' inverse TCC evaluation. Defaults is (-5,5).
#' @param cut_scale Character string indicating the scale of routing cuts.
#'   Must be one of \code{"score"} or \code{"theta"}. Default is "score".
#' @param cuts List of routing cuts, as required by
#' \code{\link{analytic_mst_precision}}.
#' @param tol A convergence tolerance used for inverse TCC calculations. Default is 1e-4.
#'
#' @details
#'
#' \enumerate{
#'   \item Item parameters are used to compute module-level ICC on the
#'   specified ability grid.
#'   \item The resulting ICCs are passed to
#'   \code{\link{analytic_mst_precision}} for analytic evaluation.
#' }
#'
#' Users who require finer control over ICC construction (e.g., custom ability
#' grids, or item-level parameter handling)
#' should use \code{\link{analytic_mst_precision_items}}.
#'
#' @return The same list returned by \code{\link{analytic_mst_precision}}.
#'
#' @seealso [compute_icc()],[module_score_dist()],[joint_module_score_dist()],[normalize_cut_scores()],[inv_tcc_from_icc()]
#'
#' @references
#' Lim, H., Davey, T., & Wells, C. S. (2020).A recursion-based analytical approach to evaluate the performance of MST.
#' \emph{Journal of Educational Measurement}, 58(2), 154--178.\href{https://doi.org/10.1111/jedm.12276}{doi:10.1111/jedm.12276}
#'
#'
#' @export

analytic_mst_precision_items <- function(design,
                                         assembled_panel,
                                         item_par_cols,model_col,
                                         D = 1,
                                         theta = seq(-3,3,0.1), range_tcc = c(-5,5),
                                         cuts,cut_scale = "score",tol = 1e-4){
  ## ---- require validated assembled panel ----
  if (!inherits(assembled_panel, "mstATA_panel")) {
    stop("Input 'assembled_panel' must be an object of class 'mstATA_panel'.",
         call. = FALSE)
  }
  cut_scale<-match.arg(cut_scale,choices = c("score","theta"))

  out <- lapply(names(assembled_panel), function(panel_name) {

    panel <- assembled_panel[[panel_name]]
    items_in_modules<-panel$ItemsInModules
    icc_by_mod<-compute_icc_by_mod(items_in_modules = items_in_modules,item_par_cols = item_par_cols,
                                   model_col = model_col,theta = theta,D = D)
    analytic_mst_precision(design = design,icc_by_mod = icc_by_mod,cut_scale = "theta",
                           cuts = cuts,range_tcc = range_tcc,tol = tol)
  })
  names(out)<-names(assembled_panel)
  return(out)
}

