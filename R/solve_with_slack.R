#' @title Solve an infeasible mstATA model using block-level slack variables
#'
#' @description
#' Attempts to recover feasibility for an infeasible \code{mstATA_model} by
#' relaxing selected non-core constraint blocks using slack variables with
#' user-specified penalties. A single continuous slack variable is introduced
#' for each relaxable constraint block, allowing controlled violations while
#' preserving the core structure of the MST design.
#'
#' This function is intended as a diagnostic and fallback tool rather than a
#' replacement for strict feasibility checks.
#'
#' @param model_spec An object of class \code{mstATA_model}. The model may be
#'   infeasible under its original specification.
#'
#' @param solver A character string indicating which solver to use.
#'   One of \code{"gurobi"}, \code{"lpsolve"}, \code{"HiGHS"},
#'   \code{"Symphony"}, or \code{"GLPK"}.
#'
#' @param time_limit The maximum amount of computation time allocated to the solver.
#' Default is 99999 seconds.
#'
#' @param cat_penalty Numeric scalar giving the penalty coefficient applied to
#'   slack variables associated with categorical constraint blocks. Larger values
#'   discourage violations of categorical requirements.
#'
#' @param quant_penalty Numeric scalar giving the penalty coefficient applied to
#'   slack variables associated with quantitative constraint blocks.
#'
#' @details
#' The relaxed model is constructed as follows:
#'
#' \itemize{
#'   \item \strong{Hard constraints (never relaxed):}
#'   \itemize{
#'     \item Objective-related constraints.
#'     \item Essential MST structure constraints (e.g., module- and pathway-level
#'           item count constraints, optional requirements for routing decision points).
#'     \item Logical constraints (e.g., enemy relationships, item-stimulus grouping relationships, item exposure control).
#'     \item Equality constraints, identified by
#'           \code{specification$Operator == "(exact)"} at the block level.
#'   }
#'
#'   \item \strong{Soft constraints (relaxed with slack):}
#'   \itemize{
#'     \item Categorical constraint blocks.
#'     \item Quantitative constraint blocks.
#'   }
#' }
#'
#' For each relaxable constraint block, one nonnegative continuous slack variable
#' is introduced and shared across all rows in that block. Slack coefficients are
#' assigned according to the row operator:
#'
#' \itemize{
#'   \item For \code{"<="} rows, the slack enters with coefficient \eqn{-1}.
#'   \item For \code{">="} rows, the slack enters with coefficient \eqn{+1}.
#' }
#'
#' Soft constraint blocks must contain only \code{"<="} or \code{">="} rows.
#'
#' The objective function is augmented by adding penalty terms for each slack
#' variable. Categorical slack variables are penalized **more heavily** than
#' quantitative slack variables to reflect their higher priority.
#'
#' @section Mathematical Formulation:
#'
#' Consider an infeasible mstATA model consisting of a set of constraint blocks
#' indexed by \eqn{b = 1,\dots,B}. Each block \eqn{b} corresponds to one row in
#' the specification table and may contain multiple linear constraint rows
#' indexed by \eqn{r \in \mathcal{R}_b}.
#'
#' For each constraint row, let \eqn{a_r^\top x} denote the left-hand side
#' linear expression and \eqn{d_r} the right-hand side constant.
#'
#' \strong{Soft constraint blocks and slack variables.}
#' For each relaxable constraint block \eqn{b} (categorical or quantitative),
#' a single nonnegative continuous slack variable \eqn{s_b \ge 0} is introduced
#' and shared across all rows in that block.
#'
#' \strong{Original (hard) constraints.}
#' For a row \eqn{r \in \mathcal{R}_b}, the original linear constraint takes one
#' of the following forms:
#'
#' \itemize{
#'   \item \strong{Upper-bound constraint:}
#'     \deqn{a_r^\top x \le d_r}
#'
#'   \item \strong{Lower-bound constraint:}
#'     \deqn{a_r^\top x \ge d_r}
#' }
#'
#' Equality constraints (\eqn{a_r^\top x = d_r}) are not relaxable and therefore
#' are excluded from slack construction.
#'
#' \strong{Relaxed constraints with block-level slack.}
#' For a relaxable block \eqn{b}, each row \eqn{r \in \mathcal{R}_b} is modified
#' as follows:
#'
#' \itemize{
#'   \item If the original row is \eqn{a_r^\top x \le d_r}, the relaxed form is
#'     \deqn{a_r^\top x \le d_r + s_b.}
#'
#'   \item If the original row is \eqn{a_r^\top x \ge d_r}, the relaxed form is
#'     \deqn{a_r^\top x \ge d_r - s_b.}
#' }
#'
#' Equivalently, in matrix form, the slack variable \eqn{s_b} enters each row
#' with coefficient \eqn{-1} for \code{"<="} constraints and \eqn{+1} for
#' \code{">="} constraints. A single slack variable therefore bounds the maximum
#' violation across all rows in the block.
#'
#' \strong{Categorical vs quantitative constraints.}
#' The mathematical relaxation is identical for categorical and quantitative
#' constraint. They differ only in the penalty applied to their slack
#' variables in the objective function. Categorical constraints are penalized
#' more heavily to reflect higher priority.
#'
#' \strong{Objective function with slack penalties.}
#' Let \eqn{\mathcal{B}_{\text{cat}}} and \eqn{\mathcal{B}_{\text{quant}}} denote
#' the sets of categorical and quantitative soft constraint blocks, respectively.
#' The relaxed optimization problem augments the original objective by adding
#' slack penalties:
#'
#' \deqn{
#' \min \; f(x)
#' + \sum_{b \in \mathcal{B}_{\text{cat}}} \lambda_{\text{cat}}\, s_b
#' + \sum_{b \in \mathcal{B}_{\text{quant}}} \lambda_{\text{quant}}\, s_b,
#' }
#'
#' where \eqn{f(x)} denotes the original objective function, and
#' \eqn{\lambda_{\text{cat}} > \lambda_{\text{quant}} > 0} are user-specified
#' penalty coefficients.
#'
#' If the original problem is a maximization problem, the equivalent formulation
#' is obtained by subtracting the slack penalties from the objective:
#'
#' \deqn{
#' \max \; f(x)
#' - \sum_{b \in \mathcal{B}_{\text{cat}}} \lambda_{\text{cat}}\, s_b
#' - \sum_{b \in \mathcal{B}_{\text{quant}}} \lambda_{\text{quant}}\, s_b.
#' }
#'
#' The resulting solution represents a best-compromise feasible design that
#' minimizes violations of high-priority constraints before lower-priority ones.
#'
#' @return
#' A solver result object returned by \code{solve_model()}, containing the
#' optimized solution, solver status, and objective value for the relaxed model.
#' Slack variable values indicate the magnitude of constraint violations at the
#' block level.
#'
#' @seealso
#' \code{\link{check_singleblock_feasibility}},
#' \code{\link{check_comblock_feasibility}},
#' \code{\link{solve_model}}
#'
#' @export

solve_with_slack<-function(model_spec,cat_penalty,quant_penalty,
                           solver = c("gurobi","lpsolve","HiGHS","Symphony","GLPK"),
                           time_limit = 99999){
  if (!inherits(model_spec, "mstATA_model")) {
    stop("'model_spec' must be a 'mstATA_model'object.",call. = FALSE)
  }
  operators<-model_spec$operators
  A_real<-model_spec$A_real
  num_constraints<-nrow(model_spec$A_binary)

  decisionvar_name<-model_spec$varname
  decisionvar_type<-model_spec$vtype
  lb_bound<-model_spec$lb[decisionvar_type!="B"]
  ub_bound<-model_spec$ub[decisionvar_type!="B"]
  spec<-model_spec$specification

  objective_block<-which(spec$Source == "Objective")
  strict_spec<-c("(MST structure) Number of items in each pathway and module min/max.",
                 "(MST structure) Number of items in each module.",
                 "(MST structure) RDP")
  strict_block<-which(spec$Requirement%in%strict_spec)
  logic_blocks<-which(spec$Type=="Logical")
  hard_blocks<-which(spec$Operator=="(exact)")
  required_blocks<-sort(unique(c(strict_block,logic_blocks,objective_block,hard_blocks)))
  cat_blocks<-setdiff(which(spec$Type=="Categorical"),required_blocks)
  quant_blocks<-setdiff(which(spec$Type=="Quantitative"),required_blocks)

  if (length(cat_blocks) + length(quant_blocks) == 0) {
    stop("No relaxable constraint blocks detected.",
         call. = FALSE)
  }
  soft_blocks <- c(cat_blocks, quant_blocks)
  soft_rows <- unlist(lapply(soft_blocks,function(i) spec$Row_Start[i]:spec$Row_End[i]))
  if (any(operators[soft_rows] == "=")) {
    stop(
      "Equality constraints detected in soft (categorical or quantitative) blocks. ",
      "Equality constraints must be labeled as '(exact)' and are not relaxable.",
      call. = FALSE
    )
  }

  num_slack<-length(cat_blocks)+length(quant_blocks)
  decisionvar_name_new <- c(paste0("slack[", cat_blocks, "]"),
                            paste0("slack[", quant_blocks, "]"))
  C_real_add<-c(rep(cat_penalty,length(cat_blocks)),
                rep(quant_penalty,length(quant_blocks)))

  Slack_C<-Matrix::Matrix(0,nrow = num_constraints, ncol = num_slack,sparse = TRUE)
  spec[,"Relaxable"]<-TRUE
  spec[required_blocks,"Relaxable"]<-FALSE
  decisionvar_type_new<-rep("C",num_slack)

  for(i in seq_along(soft_blocks)){
    which_block<-soft_blocks[i]
    block_rows<-spec$Row_Start[which_block]:spec$Row_End[which_block]
    le <- block_rows[operators[block_rows] == "<="]
    ge <- block_rows[operators[block_rows] == ">="]
    if(length(le)>=1){
      Slack_C[le, i] <- -1
    }
    if(length(ge)>=1){
      Slack_C[ge, i] <-  1
    }
  }

  if(model_spec$sense=="max"){
    C_real_add<- -1*C_real_add
  }
  A_real_new<-cbind(A_real,Slack_C)

  relaxed_model<-create_model(name = model_spec$name,specification = spec,
                              A_binary = model_spec$A_binary,
                              A_real = A_real_new,
                              C_binary = model_spec$C_binary,
                              C_real = c(model_spec$C_real,C_real_add),
                              operators = model_spec$operators,d = model_spec$d,
                              decisionvar_name = c(decisionvar_name,decisionvar_name_new),
                              decisionvar_type = c(decisionvar_type,decisionvar_type_new),
                              sense = model_spec$sense,
                              lb_bound = c(lb_bound,rep(0,num_slack)),
                              ub_bound = c(ub_bound,rep(Inf,num_slack)))
  new_out<-solve_model(model_spec = relaxed_model,solver = solver,
                       continuous_bound_source = "none",continuous_bounds = NULL,
                       time_limit = time_limit,
                       check_feasibility = FALSE,
                       show_progress = FALSE)
  if(!is.null(new_out$solution$best_solution)){
    varname<-new_out$model$varname
    dv_values<-new_out$solution$best_solution
    ## extract slack variable values
    slack_idx <- varname %in% decisionvar_name_new
    slack_vals <- dv_values[slack_idx]

    ## identify relaxed blocks (positive slack)
    relaxed_blocks <- soft_blocks[slack_vals > 1e-10]
    spec[,"Relaxed"]<-FALSE
    spec[relaxed_blocks, "Relaxed"]<-TRUE
    new_out$model$specification<-spec

    if (length(relaxed_blocks) > 0) {
      message(
        paste0(
          length(relaxed_blocks),
          " constraint block(s) are relaxed in the solution. ",
          "Each slack value represents the magnitude of the worst violation ",
          "within the corresponding block. ",
          "See the returned object for full slack details."
        )
      )
    } else {
      message(
        "No constraint blocks are relaxed in the solution. ",
        "All soft constraints are satisfied without violation."
      )
    }
  }
  return(new_out)
}
