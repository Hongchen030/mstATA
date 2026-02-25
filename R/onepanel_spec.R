#' @title Define a Single-Panel mstATA Model
#'
#' @description
#'
#' Constructs a **single-panel ATA specification** by assembling constraints,
#'  optional objective formulations, and required decision-variable
#' linking rules into a solver-ready model with full semantic traceability.
#'
#' This function validates that all supplied constraints apply to a *single*
#' panel, automatically injects decision-variable linking constraints when
#' required, and records row-index ranges that map semantic requirements to
#' solver constraint rows.
#'
#' @param x An object of class \code{"mstATA_design"} created by \code{mst_design()}.
#' @param constraints A list of \code{"mstATA_constraint"} objects defining
#'   requirements for assembling **one panel**.
#'   Constraints that explicitly reference multiple panels (i.e., solution-level
#'   constraints across panels) are not allowed and will be rejected.
#' @param objective Optional compiled objective of class
#'   \code{"compiled_objective"}.
#'   Objectives may introduce auxiliary decision variables and additional
#'   constraints, and are only permitted for operational (single-panel)
#'   specifications.
#'
#' @details
#'
#' **1. Constraint scope**
#'
#' All constraints supplied to \code{onepanel_spec()} must apply to a
#' single panel. This includes attributes defined at the item, stimulus,
#' itemset, module, pathway, or panel level.
#'
#' Constraints that operate *across panels* (solution-level reuse or exposure
#' constraints) must be specified during multi-panel expansion and are not
#' permitted in a single-panel specification.
#'
#' **2. Automatic decision-variable linking**
#'
#' If module-level logical constraints involving item–stimulus relationships
#' are detected (e.g., \code{stim_itemquant_con}), and neither
#' \code{stim_itemcount_con} nor \code{stim_itemcat_con} is included,
#' \code{onepanel_spec()} automatically injects item–stimulus gating constraints
#' via all-in/all-out selection.
#'
#' This ensures that item-level and stimulus-level selections remain
#' logically consistent. In other words, when constraints reference
#' item–stimulus relationships without explicitly enforcing selection linkage,
#' the required decision-variable linking constraints are added implicitly.
#'
#'
#' **3. Row-index traceability**
#'
#' All constraint rows—including those introduced by objectives and
#' automatically injected decision-variable linking constraints—are assigned
#' contiguous row-index ranges in the compiled constraint matrix.
#'
#' These ranges are recorded in the returned \code{specification} table,
#' allowing users to:
#'
#' \itemize{
#'   \item diagnose infeasibility,
#'   \item identify binding constraint groups, and
#'   \item programmatically relax or prioritize subsets of constraints.
#' }
#'
#'
#' @return An object of class \code{"mstATA_model"}, represented as a named list
#' containing constraint matrices, objective coefficients, decision variable
#' metadata, bounds, and optimization sense.
#' \describe{
#'   \item{name}{A character vector indicating the specifications in each row of `A_binary`}
#'   \item{specification}{A \code{data.frame} summarizing the constraint specification, including
#'    the requirement name, attribute, constraint type, application level,
#'    operator, and the number of constraint rows generated.}
#'   \item{A_binary}{A sparse constraint matrix (class \code{"Matrix"}) for binary
#'   decision variables. Rows correspond to constraints; columns correspond to
#'   binary decision variables.}
#'   \item{A_real}{A sparse constraint matrix (class \code{"Matrix"}) for
#'   continuous decision variables. Must have the same number of rows as
#'   \code{A_binary}.}
#'   \item{A}{Constraint coefficient matrix (sparse or dense).}
#'   \item{C_binary}{A numeric vector of objective coefficients for binary decision
#'   variables.}
#'   \item{C_real}{A numeric vector of objective coefficients for continuous
#'   decision variables.}
#'   \item{obj}{Objective function coefficient vector.}
#'   \item{d}{Right-hand-side vector of constraints.}
#'   \item{operators}{Constraint operators (e.g., \code{"<="}, \code{">="}, \code{"="}).}
#'   \item{vtype}{Decision variable types (\code{"B"} for binary, \code{"C"} for continuous).}
#'   \item{varname}{Decision variable names.}
#'   \item{lb}{Lower bounds for decision variables.}
#'   \item{ub}{Upper bounds for decision variables.}
#'   \item{sense}{Optimization sense (\code{"min"} or \code{"max"}).}
#' }
#'
#' @examples
#' data("reading_itempool")
#'
#' pivot_stim_map <- create_pivot_stimulus_map(
#'   itempool   = reading_itempool,
#'   stimulus   = "stimulus",
#'   pivot_item = "pivot_item"
#' )
#'
#' test_mstATA <- mst_design(
#'   itempool = reading_itempool,
#'   design   = "1-3-3",
#'   module_length = c(14,12,12,12,12,12,12),
#'   pivot_stim_map = pivot_stim_map
#' )
#' spec1<-test_itemcount_con(x=test_mstATA)
#' spec2<-test_stimcat_con(
#'   x = test_mstATA,
#'   attribute = "stimulus_type",
#'   cat_levels = c("history","social studies"),
#'   operator = "=",
#'   target_num = c(1,1),
#'   which_module = 1)
#' spec3<-test_stimcat_con(
#'   x = test_mstATA,
#'   attribute = "stimulus_type",
#'   cat_levels = "history",
#'   operator = "=",
#'   target_num = 2,
#'   which_module = 2:4)
#' spec4<-test_stimcat_con(
#'   x = test_mstATA,
#'   attribute = "stimulus_type",
#'   cat_levels = "social studies",
#'   operator = "=",
#'   target_num = 2,
#'   which_module = 5:7)
#' spec5<-stim_itemcount_con(
#'   x = test_mstATA,
#'   min = 5,
#'   max = 7
#' )
#' obj_term<-objective_term(x = test_mstATA,attribute = "discrimination",
#'                          applied_level = "Module-level",which_module = 1,sense = "max")
#'
#' panel_spec <- onepanel_spec(
#'   x = test_mstATA,
#'   constraints = list(spec1,spec2,spec3,spec4,spec5),
#'   objective = single_obj(x=test_mstATA,single_term = obj_term)
#' )
#'
#'
#' @export

onepanel_spec <- function(x,constraints,objective = NULL) {

  if (!inherits(x, "mstATA_design")) {
    stop("Input 'x' must be an object of class 'mstATA_design'.")
  }

  if (!is.list(constraints) ||
      !all(vapply(constraints, inherits, logical(1), "mstATA_constraint"))) {
    stop("`constraints` must be a list of 'mstATA_constraint' objects.", call. = FALSE)
  }

  for (con in constraints) {
    if (is_multipanel_solution_constraint(constraint = con,x = x)) {
      stop(
        "Solution-level constraint '", con$name,
        "' is defined over multiple panels and cannot be used in onepanel_spec().",
        call. = FALSE
      )
    }
  }


  if (needs_dvlink_item_stim(constraints)) {
    pivot_stim_map<-x$pivot_stim_map
    if(is.null(pivot_stim_map)){
      stop(
        "Stimulus-based constraints require `pivot_stim_map`. ",
        "Create it using `create_pivot_stimulus_map()` and supply it via `mst_design()`.",
        call. = FALSE
      )
    }
    constraints <- c(constraints,
                     list(stim_itemcount_con(x = x,min = min(pivot_stim_map$numItems_stimulus),
                                             max = max(pivot_stim_map$numItems_stimulus))))
  }

  row_cursor<-0L
  spec_con<-do.call(rbind,lapply(constraints,FUN = function(con){
    out <- add_row_ranges_from_spec(con$specification, base_row = row_cursor)
    row_cursor <<- tail(out$Row_End, 1L)   # advance cursor
    cbind(out, Source = "Constraint")
  }))
  if(!is.null(objective)){
    if(!inherits(objective, "compiled_objective")) {
      stop("`objective` must be a 'compiled_objective' object.", call. = FALSE)
    }
    spec_obj <- cbind(
      add_row_ranges_from_spec(objective$specification, base_row = row_cursor),
      Source = "Objective"
    )
    row_cursor <- tail(spec_obj$Row_End, 1L)
  }


  decisionvar_type<-x$decisionvar_type
  decisionvar_name<-x$decisionvar_name
  n_bin<-length(decisionvar_name)
  dv_bin_single <- decisionvar_name[decisionvar_type=="B"]
  dv_bin_extended <- expand_binary_dv_to_panels(dv_bin_single, num_panels = 1)

  constraints_combined <- combine_constraints(constraints)
  num_con_rows<-nrow(constraints_combined$A_binary)

  ## ---- CASE 1: no objective ----
  if (is.null(objective)) {
    Specification<-spec_con
    panel_model <- create_model(name = constraints_combined$name,
                                  specification = Specification,
                                  A_binary = constraints_combined$A_binary,
                                  A_real = NULL,
                                  C_binary = rep(1L,n_bin),
                                  C_real = NULL,
                                  operators = constraints_combined$operators,
                                  d = constraints_combined$d,
                                  decisionvar_name = dv_bin_extended,
                                  decisionvar_type = rep("B",n_bin),
                                  sense = "min",
                                  lb_bound = NULL,
                                  ub_bound = NULL)
  } else {
    Specification <- rbind(spec_con, spec_obj)
    num_obj <- length(objective$decisionvar_name_new)
    constraints_combined$A_real <- Matrix::Matrix(0,nrow = num_con_rows,ncol = num_obj,
                                                  sparse = TRUE)
    panel_model <- create_model(name = c(constraints_combined$name, objective$name),
                                  specification = Specification,
                                  A_binary = rbind(constraints_combined$A_binary,
                                                   objective$A_binary),
                                  A_real = rbind(constraints_combined$A_real,
                                                 objective$A_real),
                                  C_binary = objective$C_binary,
                                  C_real = objective$C_real,
                                  operators = c(constraints_combined$operators, objective$operators),
                                  d = c(constraints_combined$d, objective$d),
                                  decisionvar_name = c(dv_bin_extended,
                                                       objective$decisionvar_name_new),
                                  decisionvar_type = c(rep("B",n_bin),
                                                       objective$decisionvar_type_new),
                                  sense = objective$sense,
                                  lb_bound = objective$notes$y_bounds[, 1],
                                  ub_bound = objective$notes$y_bounds[, 2])
  }

  return(panel_model)
}
