#' Construct MST Structural Constraints for an MST Panel
#'
#' Translates the structural specifications of a multistage testing (MST)
#' panel into a set of mandatory linear constraints that must be satisfied
#' regardless of the assembly strategy (e.g., bottom-up, top-down, or hybrid).
#'
#' The resulting constraints enforce:
#' \itemize{
#'   \item Module-level item count requirements or
#'         Pathway-level item count + module min/max + equal module length within each stage requirements
#'   \item Information balance at routing decision points (RDPs), if specified.
#' }
#'
#' These constraints are intrinsic to the MST panel configuration and are
#' independent of the assembly strategy.
#'
#' @param x An object of class \code{"mstATA_design"} created by \code{mst_design()}.
#'
#' @param stage_length_bound Optional bounds on the total number of items
#' in each stage, passed to \code{\link{test_itemcount_con}}.
#'
#' @param info_tol A single positive numeric value specifying the allowable
#' difference in information between adjacent modules at each routing decision
#' point.
#'
#' @details
#' This function bundles structural constraints that are required for all MST
#' panel assembly problems.
#'
#' When routing constraints are requested, the item pool must already contain
#' item-level information functions of the form
#' \code{iif(theta = theta_point)}. These can be prepared using
#' \code{\link{compute_iif}}.
#'
#' @return An object representing a combined set of linear constraints,
#' typically of class \code{mstATA_constraint_set}.
#'
#' @seealso \code{\link{mst_design}},
#'   \code{\link{test_itemcount_con}},
#'   \code{\link{test_rdp_con}}
#'
#' @examples
#' data("mini_itempool")
#' mini_itempool[,paste0("iif(theta=",c(-0.5,0.5),")")]<-compute_iif(mini_itempool,
#' item_par_cols = list("3PL"=c("discrimination","difficulty","guessing")),
#'                                                      theta = c(-0.5,0.5),model_col = "model")
#' test_mstATA <- mst_design(
#'   itempool = mini_itempool,module_length = c(3,4,4,4),
#'   design = "1-3",rdp = list(c(-0.5,0.5))
#' )
#' ## build mandatory structural constraints: module-level item count and rdp constraint
#' mst_structure_con(x = test_mstATA,info_tol = 0.4)
#' @export

mst_structure_con<-function(x,stage_length_bound = NULL,info_tol = 0.5){
  if (!inherits(x, "mstATA_design")) {
    stop("Input 'x' must be an object of class 'mstATA_design'.",
         call. = FALSE)
  }
  NumStages<-x$NumStages
  RDP<-x$RDP
  con1<-test_itemcount_con(x = x,stage_length_bound = stage_length_bound)

  if (!is.null(RDP)) {
    con2<-list()
    for(s in seq_len(NumStages-1)){
      con2[[s]]<-test_rdp_con(x = x,rdp = RDP[[s]],which_stage = s,info_tol = info_tol)
    }
    con2_final<-combine_constraints(con2)
  return(combine_constraints(list(con1,con2_final)))
  }else{
    return(con1)
  }
}
