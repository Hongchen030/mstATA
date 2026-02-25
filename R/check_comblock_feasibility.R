#' @title Feasibility check for a specified combination of constraint blocks
#'
#' @description
#' Diagnoses infeasibility in an \code{mstATA_model} by testing whether a
#' user-specified combination of constraint blocks, together with the
#' core set of constraints, results in an infeasible model.
#'
#' The full model must be infeasible. A reduced model is constructed that
#' always includes the core constraints (objective-related constraints and
#' essential MST structure constraints) and additionally includes the
#' constraint blocks specified in \code{con_blocks}. The solver status of
#' this reduced model is then evaluated.
#'
#' @param model_spec An object of class \code{mstATA_model}. The full model
#'   must be infeasible.
#'
#' @param con_blocks An integer vector giving the indices of constraint
#'   blocks (rows of \code{model_spec$specification}) to be jointly tested.
#'   These blocks are added to the core constraints as a group.
#'
#' @param solver A character string indicating which solver to use.
#'   One of \code{"gurobi"}, \code{"lpsolve"}, \code{"HiGHS"},
#'   \code{"Symphony"}, or \code{"GLPK"}.
#'
#' @param time_limit The maximum amount of computation time allocated to the solver.
#' Default is 99999 seconds.
#'
#' @details
#' The reduced model always includes a fixed core set of constraints:
#' \itemize{
#'   \item Objective-related constraints.
#'   \item Essential MST structure constraints (e.g., module-/pathway-level
#'         item count constraints, routing decision points constraints, item reusage within
#'         a panel).
#' }
#'
#' The constraint blocks specified in \code{con_blocks} are added to this
#' core \emph{simultaneously} and tested as a group. This function does not
#' perform a one-at-a-time or incremental check; instead, it evaluates the
#' feasibility of the combined block set.
#'
#' This function is primarily intended for second-stage infeasibility
#' diagnostics, such as verifying suspected conflicting constraint
#' combinations or validating results from block-wise or additive
#' diagnostic procedures.
#'
#' @return
#' A list with the following elements:
#' \describe{
#'   \item{blocks_tested}{Character vector of requirement names corresponding
#'   to \code{con_blocks}.}
#'   \item{status}{Solver status returned for the reduced model (e.g.,
#'   \code{"INFEASIBLE"}, \code{"OPTIMAL"}, \code{"FEASIBLE"}).}
#' }
#'
#' @seealso [check_singleblock_feasibility()]
#'
#' @export
check_comblock_feasibility<-function(model_spec,con_blocks,solver,
                                     time_limit = 99999){
  if (!inherits(model_spec, "mstATA_model")) {
    stop("'model_spec' must be a 'mstATA_model' object.",call. = FALSE)
  }

  spec<-model_spec$specification
  if (!all(con_blocks %in% seq_len(nrow(spec)))) {
    stop("'con_blocks' are not valid constraint block indices.", call. = FALSE)
  }

  objective_block<-which(spec$Source == "Objective")
  objective_rows <- if (length(objective_block)) {
    unlist(lapply(objective_block,FUN = function(i)  spec$Row_Start[i]:spec$Row_End[i]))
  } else integer(0)

  strict_spec<-c("(MST structure) Number of items in each pathway and module min/max.",
                 "(MST structure) Number of items in each module.",
                 "(MST structure) RDP",
                 "Item exposure control within a panel")
  strict_block<-which(spec$Requirement%in%strict_spec)
  strict_rows <- if (length(strict_block)) {
    unlist(lapply(strict_block,function(i) spec$Row_Start[i]:spec$Row_End[i]))
  } else integer(0)

  if (!length(con_blocks)) {
    warning("No constraint blocks specified; testing strict + objective only.")
  }

  if (any(con_blocks %in% objective_block)) {
    message("Some blocks in 'con_blocks' are objective-related and are always included.")
  }

  if (any(con_blocks %in% strict_block)) {
    message("Some blocks in 'con_blocks' are MST structure constraints and are always included.")
  }


  con_rows <- if (length(con_blocks)) {
    unlist(lapply(con_blocks, function(i) spec$Row_Start[i]:spec$Row_End[i]))
  } else integer(0)

  subset_rows<-sort(unique(c(strict_rows,objective_rows,con_rows)))
  if (!length(subset_rows)) {
    stop("No constraint rows selected for feasibility check.", call. = FALSE)
  }

  decisionvar_name<-model_spec$varname
  decisionvar_type<-model_spec$vtype
  lb_bound<-model_spec$lb[decisionvar_type!="B"]
  ub_bound<-model_spec$ub[decisionvar_type!="B"]

  small_model<-create_model(name = model_spec$name[subset_rows],specification = NULL,
                            A_binary = model_spec$A_binary[subset_rows,,drop=FALSE],
                            A_real = model_spec$A_real[subset_rows,,drop=FALSE],
                            C_binary = model_spec$C_binary,C_real = model_spec$C_real,
                            operators = model_spec$operators[subset_rows],d = model_spec$d[subset_rows],
                            decisionvar_name = decisionvar_name,
                            decisionvar_type = decisionvar_type,
                            sense = model_spec$sense,
                            lb_bound = lb_bound,ub_bound = ub_bound)
  sol <- solve_model(small_model, solver = solver,continuous_bound_source = "none",continuous_bounds = NULL,
                     time_limit = time_limit,check_feasibility = FALSE,show_progress = FALSE)
  status <- sol$solution$solution_found

  list(blocks_tested = paste0("model_spec$specification block: ",paste(con_blocks,collapse = " + ")),
       status = status)
}
