#' @title Block-wise feasibility diagnostics for infeasible mstATA models
#'
#' @description
#' Diagnoses infeasibility in an \code{mstATA_model} by solving a sequence of
#' reduced models. The full model must be infeasible. A core set of constraints
#' (objective-related constraints and essential MST structure
#' constraints) is checked for feasibility first. If feasible, the core set of
#' constraints is always included. Each remaining constraint block is then tested
#' individually by solving a reduced model that contains the core constraints plus
#' exactly one additional constraint block (i.e., non-core blocks are not accumulated).
#'
#'
#' @param model_spec An object of class \code{mstATA_model}. The model must be
#'   infeasible.
#'
#' @param solver A character string indicating which solver to use.
#'   One of \code{"gurobi"}, \code{"lpsolve"}, \code{"HiGHS"},
#'   \code{"Symphony"}, or \code{"GLPK"}.
#'
#' @param time_limit The maximum amount of computation time allocated to the solver.
#' Default is 99999 seconds.
#'
#' @return
#' A data frame with one row per constraint block tested.
#'
#' @details
#' The core model always includes:
#' \itemize{
#'   \item Objective-related constraints.
#'   \item Essential MST structure constraints (e.g., module-/pathway-level
#'         item count constraints, routing decision points constraints, item reusage within
#'         a panel).
#' }
#'
#' The MST structure constraints ensure that the fundamental structural requirements of the MST design are
#' satisfied.
#'
#' Specifically, MST structure constraints include:
#'
#' \strong{(1) Item count constraints}
#'
#' (a) Number of items in each module is provided in \code{mst_design()}
#'
#' \itemize{
#'   \item One constraint is created for each module.
#'   \item Each module must contain \emph{exactly} the specified number of items.
#' }
#'
#' (b) Number of items in each pathway is provided in \code{mst_design()}.
#'
#' \itemize{
#'   \item Each pathway must contain \emph{exactly} the specified number of items.
#'   \item Optional stage-level structure constraints may additionally enforce:
#'   \itemize{
#'     \item Minimum number of items per stage.
#'     \item Maximum number of items per stage.
#'   }
#'   \item If \code{stage_length_bound = NULL}, the following default stage-level
#'         structure is enforced:
#'   \itemize{
#'     \item Each stage must contain at least one item.
#'     \item Modules within the same stage must have equal length.
#'   }
#' }
#'
#' \strong{(2) Routing decision points constraints}
#'
#' These constraints are generated when \code{rdp} is provided in
#' \code{mst_design()}.
#'
#' \itemize{
#'   \item The test information at the routing decision point between adjacent
#' modules in the next stage is similar within a specified tolerance.
#' }
#'
#' \strong{(3) Panel-level item usage constraints}
#'
#' These constraints are generated using \code{panel_itemreuse_con()} limiting
#' how many times an item may be selected across the modules and pathways contained within a single MST panel.
#'
#' After checking the feasibility of the core set of constraints, all
#' other (non-structural) constraint blocks are tested individually by adding
#' them to the core model one at a time. This function does not attempt to diagnose
#' infeasibility arising from interactions among multiple non-core constraint
#' blocks.
#'
#' @seealso
#' [test_itemcount_con()],[test_rdp_con()],[mst_structure_con()],[panel_itemreuse_con()],[check_comblock_feasibility],[solve_model()]
#'
#' @export
#'
check_singleblock_feasibility<-function(model_spec,solver,
                                        time_limit = 99999){
  if (!inherits(model_spec, "mstATA_model")) {
    stop("'model_spec' must be a 'mstATA_model' object.",call. = FALSE)
  }

  name<-model_spec$name
  A_binary<-model_spec$A_binary
  A_real<-model_spec$A_real
  C_binary<-model_spec$C_binary
  C_real<-model_spec$C_real
  operators<-model_spec$operators
  d<-model_spec$d
  decisionvar_name<-model_spec$varname
  decisionvar_type<-model_spec$vtype
  lb_bound<-model_spec$lb[decisionvar_type!="B"]
  ub_bound<-model_spec$ub[decisionvar_type!="B"]
  sense<-model_spec$sense

  spec<-model_spec$specification
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

  required_blocks<-sort(unique(c(strict_block,objective_block)))
  required_rows <- sort(unique(c(strict_rows, objective_rows)))

  mini_model<-create_model(name = name[required_rows],specification = NULL,
                            A_binary = A_binary[required_rows,,drop=FALSE],
                            A_real = A_real[required_rows,,drop=FALSE],
                            C_binary = C_binary,C_real = C_real,
                            operators = operators[required_rows],d = d[required_rows],
                            decisionvar_name = decisionvar_name,decisionvar_type = decisionvar_type,
                            sense = sense,
                            lb_bound = lb_bound,ub_bound = ub_bound)
  mini_sol <- solve_model(mini_model, solver = solver,continuous_bound_source = "none",continuous_bounds = NULL,
                     time_limit = time_limit,check_feasibility = FALSE,show_progress = FALSE)

  mini_status <- mini_sol$solution$solution_found
  if(!mini_status%in%c("FEASIBLE","OPTIMAL")){
    message("The ATA model that includes only the MST structure and objective is not feasible.")
  }

  flexible_block<-setdiff(which(spec$Source == "Constraint"),strict_block)
  if (!length(flexible_block)) {
    message("No flexible constraint blocks found.")
    return(data.frame(
      Requirement = character(),
      Attribute = character(),
      Type = character(),
      `Application Level` = character(),
      Operator = character(),
      `Num of Constraints` = numeric(),
      Row_Start = numeric(),
      Row_End = numeric(),
      Source = character(),
      Status = character(),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }

  results <- vector("list", length(flexible_block))

  for(i in seq_along(flexible_block)){
    con_block<-flexible_block[i]
    con_block_rows<-spec$Row_Start[con_block]:spec$Row_End[con_block]
    subset_rows <- sort(unique(c(con_block_rows, required_rows)))
    small_model<-create_model(name = name[subset_rows],specification = NULL,
                              A_binary = A_binary[subset_rows,,drop=FALSE],A_real = A_real[subset_rows,,drop=FALSE],
                              C_binary = C_binary,C_real = C_real,
                              operators = operators[subset_rows],d = d[subset_rows],
                              decisionvar_name = decisionvar_name,decisionvar_type = decisionvar_type,
                              sense = sense,
                              lb_bound = lb_bound,ub_bound = ub_bound)
    sol <- solve_model(small_model, solver = solver,continuous_bound_source = "none",continuous_bounds = NULL,
                       time_limit = time_limit,check_feasibility = FALSE,show_progress = FALSE)

    status <- sol$solution$solution_found
    results[[i]]<-c(as.vector(as.matrix(spec[con_block,])),status)
  }
  flex_results<-do.call(rbind,results)
  colnames(flex_results)<-c(colnames(spec),"Status")
  Core_set<-spec[required_blocks,]
  Core_set$Requirement<-paste0("Core set: ",Core_set$Requirement)
  Core_set[,"Status"]<-mini_status

  return_results<-rbind(flex_results,Core_set)
  return(return_results)
}
