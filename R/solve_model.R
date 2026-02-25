#' @title Solve an ATA Model Using a Mathematical Programming Solver
#'
#' @description
#' Solves an automated test assembly (ATA) model in the mstATA
#' framework. The input model specification is optionally presolved
#' to remove redundant rows and columns, checked for feasibility, and then
#' solved using the requested solver.
#'
#' @param model_spec An assembly model specification of class
#'   \code{"mstATA_model"}, created by \code{onepanel_spec()} or
#'   \code{multipanel_spec()}.
#' @param continuous_bound_source Character string specifying how lower and
#'   upper bounds for continuous decision variables are determined.
#'   Must be one of \code{"compiled"}, \code{"user"}, or \code{"none"}.
#'   \describe{
#'     \item{\code{"compiled"}}{Use bounds derived from compiled objective
#'       functions.}
#'     \item{\code{"user"}}{Use user-specified bounds provided through
#'       \code{continuous_bounds}.}
#'     \item{\code{"none"}}{Do not impose explicit bounds on continuous
#'       variables; lower and upper bounds are set to \eqn{-\infty} and
#'       \eqn{\infty}, respectively.}
#'   }
#'   The default is \code{"compiled"}.
#'
#' @param continuous_bounds A named list specifying user-defined bounds for
#'   continuous decision variables when
#'   \code{continuous_bound_source = "user"}.
#'   The list must contain two numeric vectors:
#'   \describe{
#'     \item{\code{lb}}{Lower bounds for continuous variables.}
#'     \item{\code{ub}}{Upper bounds for continuous variables.}
#'   }
#'   The length of each vector must equal the number of continuous variables (not slack variables)
#'   in the model. Each element of \code{lb} must be less than or equal to the
#'   corresponding element of \code{ub}. This argument must be \code{NULL}
#'   when \code{continuous_bound_source} is \code{"compiled"} or
#'   \code{"none"}.
#' @param solver A character string indicating which solver to use.
#'   One of \code{"gurobi"}, \code{"lpsolve"}, \code{"HiGHS"},
#'   \code{"Symphony"}, or \code{"GLPK"}.
#' @param time_limit The maximum amount of computation time allocated to the solver.
#' Default is 99999 seconds.
#' @param check_feasibility Logical. Default is \code{FALSE}. If \code{TRUE} and
#'   the HiGHS package is available, HiGHS is used to check whether the model
#'   is infeasible or unbounded before running the requested solver.
#'   If HiGHS certifies the model as
#'   \code{"INFEASIBLE"} or \code{"UNBOUNDED"}, the solving process terminates
#'   early and the result is returned immediately. If HiGHS does not certify
#'  feasibility within the time limit, the specified solver is invoked.
#'  This pre-check is purely diagnostic and does not affect the final solution
#'  when the model is feasible.
#' @param show_progress Logical. Whether to display solver progress information (e.g., incumbent
#'  solutions, node counts, and optimality gap) during optimization. Note that the level of
#'  detail depends on the underlying solver. Default is \code{FALSE}.
#' @details
#'
#' The feasibility pre-check is designed to save computation time in large
#' or complex ATA models, especially when commercial solvers (e.g., Gurobi)
#' are unavailable. HiGHS is used as a fast certifier with a short time limit;
#' certification of infeasibility or unboundedness is guaranteed, whereas
#' failure to certify feasibility does not imply infeasibility.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{solution}{A list containing the solver results with the
#'     following elements:
#'      \describe{
#'       \item{solution_found}{A character string indicating the solver status
#'       (e.g., \code{"OPTIMAL"}, \code{"FEASIBLE"}, or \code{"INFEASIBLE"},
#'       or \code{"UNBOUNDED"} or \code{"TIME_LIMIT"} or \code{"OTHER"}).}
#'       \item{best_solution}{A numeric vector of decision variable values
#'       corresponding to the best solution found.}
#'       \item{objval}{The objective function value associated with
#'       \code{best_solution}.}
#'       \item{solution_list}{Either \code{NULL} or a list of optimal solutions.
#'       If multiple optimal solutions exist, only the \code{"gurobi"} solver
#'       may return more than one solution.}
#'       \item{check}{Either \code{NULL} or a data frame summarizing constraint
#'       evaluation at the solution, including left-hand side values,
#'       constraint operators, right-hand side values, and residual values.}
#'       \item{runtime}{The total runtime of the solver in seconds.}
#'       \item{iis}{If \code{solver = "gurobi"} and
#'   model is infeasible, an irreducible inconsistent subsystem (IIS) will also be returned.
#'   It should be mentioned that an infeasible model may have multiple IISs.
#'   The one returned by Gurobi is not necessarily the smallest one;
#'   there may exist others with fewer constraints or bounds.}
#'   }
#'   }
#'   \item{model}{An object of class \code{"mstATA_model"} representing the
#'     MILP formulation used by the solver. This includes the constraint
#'     matrix \code{A}, right-hand side \code{rhs}, constraint senses,
#'     objective coefficients, bounds, variable types, variable names,
#'     model sense, and row names.}
#'   \item{solver}{A character string indicating the solver used (as requested
#'     by \code{solver}).}
#' }
#' @export
#'

solve_model <- function(model_spec,
                        continuous_bound_source = "compiled",
                        continuous_bounds = NULL,
                        solver = c("gurobi","lpsolve","HiGHS","Symphony","GLPK"),
                        time_limit = 99999,
                       check_feasibility = FALSE,
                        show_progress = FALSE) {

  solver <- match.arg(solver,
                      choices = c("gurobi","lpsolve","HiGHS","Symphony","GLPK"))
  if(show_progress){
    OutputFlag<-1 # gurobi
    log_to_console <-TRUE # HiGHS
    verbosity <- 1 # Symphony
    verbose <- TRUE # GLPK
  }else{
    OutputFlag<-0
    log_to_console<-FALSE
    verbosity <- -2
    verbose<-FALSE
  }

  if (!inherits(model_spec, "mstATA_model")) {
    stop("'model_spec' must be a 'mstATA_model'object.",call. = FALSE)
  }

  if (!is.numeric(time_limit)) {
    stop("`time_limit` must be numeric.", call. = FALSE)
  }
  if (!is.finite(time_limit) | time_limit<0) {
    stop("`time_limit` must be a finite positive value.",
         call. = FALSE)
  }

  continuous_bound_source <- match.arg(continuous_bound_source,
                                       choices = c("compiled","user","none"))
  num_var<-length(model_spec$vtype)
  lb <- rep(-Inf, num_var)
  ub <- rep( Inf,num_var)
  which_binary<-which(model_spec$vtype=="B")
  lb[which_binary] <- 0
  ub[which_binary] <- 1
  is_slack <- grepl("^slack\\[[0-9]+\\]$", model_spec$varname) & model_spec$vtype == "C"
  lb[is_slack] <- 0
  ub[is_slack] <- Inf

  which_real<-setdiff(seq_len(num_var),c(which_binary,which(is_slack)))
  num_objreal<-length(which_real)
  if(continuous_bound_source == "compiled"){
    if (!is.null(continuous_bounds)) {
      stop(
        "'continuous_bounds' must be NULL when ",
        "continuous_bound_source = '", continuous_bound_source, "'.",
        call. = FALSE
      )
    }
    lb <- model_spec$lb
    ub <- model_spec$ub
  }else if (continuous_bound_source == "user"){

    if (is.null(continuous_bounds) || !is.list(continuous_bounds)) {
      stop("'continuous_bounds' must be a list with elements 'lb' and 'ub' when ",
           "continuous_bound_source = 'user'.",
           call. = FALSE)
    }
    if (!all(c("lb", "ub") %in% names(continuous_bounds))) {
      stop("'continuous_bounds' must contain named elements 'lb' and 'ub'.",
           call. = FALSE)
    }

    lb_user <- continuous_bounds$lb
    ub_user <- continuous_bounds$ub
    if (!is.numeric(lb_user) || !is.numeric(ub_user)) {
      stop("'lb' and 'ub' in 'continuous_bounds' must be numeric vectors.",
           call. = FALSE)
    }
    if (anyNA(lb_user) || anyNA(ub_user)) {
      stop("'lb' and 'ub' cannot contain NA values.",
           call. = FALSE)
    }
    if (any(lb_user > ub_user)) {
      stop("Each element of 'lb' must be <= corresponding element of 'ub'.",
           call. = FALSE)
    }

    if (length(lb_user) !=  num_objreal || length(ub_user) !=  num_objreal) {
      stop("'lb' and 'ub' must have length equal to the number of continuous variables (",
           num_objreal, ").",
           call. = FALSE)
    }
    lb[which_real]<-lb_user
    ub[which_real]<-ub_user

  }else{
    if (!is.null(continuous_bounds)) {
      stop(
        "'continuous_bounds' must be NULL when ",
        "continuous_bound_source = '", continuous_bound_source, "'.",
        call. = FALSE
      )
    }
  }

  args <- list(
    A          = model_spec$A,
    rhs        = model_spec$d,
    sense      = model_spec$operators,
    obj        = model_spec$obj,
    lb         = lb,
    ub         = ub,
    vtype      = model_spec$vtype,
    varname    = model_spec$varname,
    modelsense = model_spec$sense,
    rowname    = model_spec$name
  )


  run_precheck <-solver != "gurobi" && isTRUE(check_feasibility) && requireNamespace("highs", quietly = TRUE)
  if (run_precheck) {
    feas_params <- list(threads = 4, time_limit = 30, presolve = "on", log_to_console = FALSE)
    fea_check <- do.call(use_highs,c(args,list(params = feas_params)))
    check_status<-fea_check$solution_found
    if (check_status == "INFEASIBLE") {
      message("Model detected as infeasible by HiGHS.")
      return(list(solution = fea_check,
                  model    = model_spec,
                  solver   = "HiGHS"))
    }

    if (check_status == "UNBOUNDED") {
      message("Model detected as unbounded by HiGHS.")
      return(list(solution = fea_check,
                  model    = model_spec,
                  solver   = "HiGHS"))
    }

    if(check_status == "OPTIMAL"){
      message("HiGHS pre-check found an optimal solution.")
      return(list(solution = fea_check,
                  model    = model_spec,
                  solver   = "HiGHS"))
    }

    if (check_status == "FEASIBLE") {
      message("HiGHS pre-check passed (feasible & bounded); proceeding to solver.")
    } else {
      message(
        "HiGHS pre-check did not certify feasibility within the time limit; ",
        "proceeding to solver."
      )
    }
  }

  params <- switch(solver,
                   gurobi = list(TimeLimit = time_limit,Threads = 4,MIPFocus = 1,Heuristics = 0.5,
                                 Presolve = 2,Cuts = 2,
                                 OptimalityTol = 1e-7,FeasibilityTol = 1e-7,MIPGap = 1e-2,
                                 OutputFlag = OutputFlag, Seed = 200,
                                 InfUnbdInfo = 1,IISMethod = -1),
                   HiGHS = list(threads = 4,time_limit = time_limit,mip_rel_gap = 1e-2,mip_abs_gap = 0.0,
                                presolve = "on",parallel = "on",log_to_console = log_to_console,
                                random_seed = 200),
                   lpsolve = list(presolve = "rows",timeout = time_limit,
                                  mip_gap = c(0,1e-2),epsb = 1e-7,epsd = 1e-7),
                   Symphony = list(verbosity = verbosity, time_limit = time_limit, gap_limit = c(0.0, 1e-2)),
                   GLPK = list(verbose = verbose, presolve = TRUE,tm_limit = time_limit*1000,
                               canonicalize_status = FALSE))


  out <- switch(solver,
                gurobi   = do.call(use_gurobi,   c(args, list(params = params))),
                HiGHS    = do.call(use_highs,    c(args, list(params = params))),
                lpsolve  = do.call(use_lpsolve,  c(args, list(params = params))),
                Symphony = do.call(use_symphony, c(args, list(params = params))),
                GLPK     = do.call(use_glpk,     c(args, list(params = params)))
  )

  return(list(solution = out,
              model = model_spec,
              solver = solver))
}



