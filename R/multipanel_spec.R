#' @title Assemble a multi-panel ATA model
#'
#' @description
#'
#' Constructs a **multi-panel ATA model** by expanding
#' single-panel specifications to multiple panels and
#' turning it into a solver-ready formulation.
#'
#' Solution-level constraints and decision-variable linking between
#' item-module-panel variables and solution-level variables are handled at this stage.
#'
#' @param x An object of class \code{"mstATA_design"} created by \code{mst_design()}.
#' @param panel_model An object of class \code{"mstATA_model"} created by \code{onepanel_spec()}.
#' @param num_panels Integer. Number of panels to be assembled.
#' @param solution_con Optional list of \code{mstATA_constraint} objects defining
#'   solution-level requirements across panels (e.g., unique
#'   item/stimulus counts across panels, unique item/stimulus counts from specific categories
#'   across panels).
#' @param global_min_use Scalar. Minimum number of times any item may be used
#'   across panels. Default = \code{0}.
#' @param global_max_use Scalar. Maximum number of times any item may be used
#'   across panels. Default = \code{Inf}.
#' @param item_min_use Optional data frame with columns \code{item_id} and
#'   \code{min}, specifying item-specific minimum reuse counts.
#' @param item_max_use Optional data frame with columns \code{item_id} and
#'   \code{max}, specifying item-specific maximum reuse counts.
#'
#'
#' @return
#' An object of class \code{"mstATA_model"}, represented as a named list
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
#' @details
#'
#' **1. Parallel Panels**
#'
#' The single-panel specification is
#' expanded to \code{num_panels} panels by replicating constraint matrices
#' in a block-diagonal structure. All panels share identical constraints
#' and objectives.
#'
#' Solution-level constraints, if provided, are appended after panel expansion.
#'
#' \strong{Specification traceability}
#'
#' The returned model contains an expanded specification table that records,
#' for each requirement:
#' \itemize{
#'   \item the originating panel,
#'   \item the number of constraints generated,
#'   \item the contiguous row ranges in the final solver matrix.
#' }
#'
#' This information can be used to diagnose infeasibility and to prioritize
#' or relax groups of constraints programmatically.
#'
#'
#' **2. Consistency between item-module-panel selection and solution-level item indicators**
#'
#' The decision-variable linking constraints (e.g., \code{dvlink_item_solution})
#' are automatically added to link item-module-panel selection and solution-level
#' item selection.
#'
#' The constraint enforces:
#'
#' \strong{
#' Each item can be used at least a specified minimum number of times and/or
#' no more than a specified maximum number of times across all panels.
#' }
#'
#' Key characteristics:
#'
#' \itemize{
#'   \item Attribute: \emph{itemset level (item-itself set)} \emph{logical} (if selected, then increment reuse).
#'   \item Constraints are applied at the \strong{"Solution-level"}.
#' }
#'
#'
#' Global reuse limits are supplied via \code{global_min_use} and
#' \code{global_max_use}. These bounds apply to all items unless overridden.
#'
#' Item-specific reuse limits may be supplied through:
#' \itemize{
#'   \item \code{item_min_use}: a data frame with columns \code{item_id} and \code{min}
#'   \item \code{item_max_use}: a data frame with columns \code{item_id} and \code{max}
#' }
#'
#' The total number of constraints generated is always
#' \eqn{2 \times \text{PoolSize}}, corresponding to one minimum and one maximum
#' constraint per item.
#'
#'
#' @seealso [onepanel_spec()]
#' @export

multipanel_spec<-function(x,panel_model,num_panels,
                          solution_con = NULL,
                          global_min_use = 0,global_max_use = Inf,
                          item_min_use = NULL,item_max_use = NULL){
  if (!inherits(x, "mstATA_design")) {
    stop("Input 'x' must be an object of class 'mstATA_design'.", call. = FALSE)
  }
  if (!inherits(panel_model, "mstATA_model")) {
    stop("For parallel panels, 'panel_model' must be a compiled single-panel mstATA_model.",
         call. = FALSE)
  }
  PoolSize<-nrow(x$ItemPool)
  x_single<-x$decisionvar_name
  decisionvar_name<-panel_model$varname
  decisionvar_type<-panel_model$vtype
  dv_bin_single <- decisionvar_name[decisionvar_type=="B"]
  x_single<-sub("\\]$", paste0(",", 1, "]"), x_single)
  if (!identical(dv_bin_single, x_single)) {
    stop("For parallel panels, 'panel_model' must correspond to a single panel with the ",
         "same decision variable structure as the base mstATA_design.",
         call. = FALSE)
  }

  if (!is.numeric(num_panels) || length(num_panels) != 1L ||
      num_panels < 1 || num_panels != as.integer(num_panels)) {
    stop("`num_panels` must be a positive integer.", call. = FALSE)
  }

  dv_bin_single<-sub("(\\w+)\\[\\s*([^,\\]]+)\\s*,\\s*([^,\\]]+)\\s*,\\s*[^\\]]+\\]",
                     "\\1[\\2,\\3]",dv_bin_single,perl = TRUE)
  dv_bin_multiple <- expand_binary_dv_to_panels(dv_bin_single, num_panels)
  dv_real<-decisionvar_name[decisionvar_type=="C"]

  A_binary_new <- blockdiag_binary(panel_model$A_binary, num_panels)
  colnames(A_binary_new)<-dv_bin_multiple
  C_binary_new<-rep(panel_model$C_binary,num_panels)
  A_real_new<-NULL
  if(!is.null(panel_model$A_real)){
    A_real_new<- expand_A_real_parallel(panel_model$A_real, num_panels)
  }
  C_real_new<-panel_model$C_real

  rhs_new <- rep(panel_model$d, num_panels)
  operators_new <- rep(panel_model$operators, num_panels)
  decisionvar_name_new <- c(dv_bin_multiple,dv_real)
  decisionvar_type_new<- c(rep("B", length(dv_bin_multiple)),
                           rep("C", length(dv_real)))
  name_new <- unlist(lapply(seq_len(num_panels), function(p)
    paste0(panel_model$name, " [P", p, "]")))

  spec_single <- panel_model$specification
  rows_per_panel <- nrow(panel_model$A_binary)
  spec_new <- expand_spec_by_panel(spec_single,rows_per_panel,seq_len(num_panels))


  # 1) add dvlink rows once (x + s)
  need_solution_layer <- !is.null(solution_con) ||global_min_use > 0 ||is.finite(global_max_use) ||!is.null(item_min_use) ||!is.null(item_max_use)
  if (need_solution_layer) {
    # 3) extend universe (x across panels + s)
    dv_solution <- paste0("s[", seq_len(PoolSize), "]")
    binary_universe <- c(dv_bin_multiple, dv_solution)
    if (anyDuplicated(binary_universe)) {
      stop("Internal error: duplicated binary decision variable names in universe.", call. = FALSE)
    }

    link_con <- dvlink_item_solution(x = x, num_panels = num_panels,
                                     global_min_use = global_min_use, global_max_use = global_max_use,
                                     item_min_use = item_min_use, item_max_use = item_max_use)

    if (!is.null(solution_con)) {
      if (!is.list(solution_con) || !all(vapply(solution_con, inherits, logical(1), "mstATA_constraint"))) {
        stop("`solution_con` must be a list of 'mstATA_constraint' objects.", call. = FALSE)
      }

      is_ok <- vapply(solution_con, is_multipanel_solution_constraint, logical(1), x = x)
      if (!all(is_ok)) {
        stop("All `solution_con` must be Solution-level constraints defined over multiple panels.", call. = FALSE)
      }

      for(con_id in 1:length(solution_con)){
        con<-solution_con[[con_id]]
        new_A<-align_binary_to_universe(con$A_binary,binary_universe)
        solution_con[[con_id]]$A_binary<-new_A
      }
      all_sol<-c(list(link_con),solution_con)
      sol_combined<-combine_constraints(all_sol)
    }else{
      sol_combined<-link_con
    }

    if (!is.null(sol_combined$A_real) && ncol(sol_combined$A_real) > 0L) {
      stop("Solution-level constraints must not introduce continuous variables.",
           call. = FALSE)
    }

    A_binary_new <- rbind(align_binary_to_universe(A_binary_new, binary_universe),
                          sol_combined$A_binary)
    C_binary_new<-c(C_binary_new,rep.int(0,PoolSize))
    if(!is.null(panel_model$A_real)){
      A_real_new <- rbind(A_real_new,
                          Matrix::Matrix(0, nrow(sol_combined$A_binary), ncol(A_real_new), sparse = TRUE))
    }
    C_real_new<-C_real_new
    operators_new<-c(operators_new,sol_combined$operators)
    rhs_new<-c(rhs_new,sol_combined$d)
    decisionvar_name_new<-c(binary_universe,dv_real)
    decisionvar_type_new<-c(rep("B", length(binary_universe)),
                            rep("C", length(dv_real)))

    if ("Row_End" %in% names(spec_new)) {
      row_cursor<-max(spec_new$Row_End, na.rm = TRUE)
    }else {
      row_cursor<-nrow(A_binary_new) - nrow(sol_combined$A_binary)
    }
    out <- add_row_ranges_from_spec(sol_combined$specification, base_row = row_cursor)
    out$Source<-"Constraint"
    out[,"Panel"]<-"Across Panels"
    col_order<-c("Source","Requirement","Attribute","Type","Application Level","Operator","Num of Constraints","Row_Start","Row_End","Panel")
    name_new<-c(name_new,sol_combined$name)
    spec_new<-rbind(spec_new[,col_order],
                    out[,col_order])
  }

  model<-create_model(name = name_new,
                      specification = spec_new,
                      A_binary = A_binary_new,
                      A_real   = A_real_new,
                      C_binary = C_binary_new,
                      C_real   = C_real_new,
                      d = rhs_new,
                      operators = operators_new,
                      decisionvar_name = decisionvar_name_new,
                      decisionvar_type = decisionvar_type_new,
                      lb_bound = panel_model$lb[decisionvar_type=="C"],
                      ub_bound = panel_model$ub[decisionvar_type=="C"],
                      sense = panel_model$sense)

  return(model)
}
