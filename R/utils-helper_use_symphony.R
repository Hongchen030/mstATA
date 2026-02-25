#' @title Solve model using symphony
#' @description Uses symphony to solve a linear or MILP model for test assembly
#' @keywords  internal
use_symphony<-function(A,rhs, sense, obj,lb = NULL, ub = NULL,
                     vtype,varname, modelsense,
                     params,rowname) {
  if (!requireNamespace("Rsymphony", quietly = TRUE)) {
    message("The 'Rsymphony' package is not installed.")
    return(NULL)
  }

  params <- params
  verbosity <- params[["verbosity"]]
  time_limit <- params[["time_limit"]]
  gap_limit <-params[["gap_limit"]]
  if(modelsense == "max"){
    max <- TRUE
  }else{
    max <-FALSE
  }

  if (!inherits(A, "sparseMatrix")) A <- Matrix::Matrix(A, sparse = TRUE)
  num_decisions<-ncol(A)
  which_equal<-which(sense=="=")
  sense[which_equal]<-"=="
  start_time<-Sys.time()
  result <- tryCatch({
    Rsymphony::Rsymphony_solve_LP(obj = obj,mat = A,dir = sense,rhs = rhs,
                                  bounds = list(lower = list(ind = seq.int(1,num_decisions),val = lb),
                                                upper = list(ind = seq.int(1,num_decisions),val = ub)),
                                  types = vtype,max = max,verbosity = verbosity,time_limit = time_limit,
                                  gap_limit = gap_limit)
  }, error = function(e) {
    message("symphony failed: ", e$message)
    return(NULL)
  })
  end_time<-Sys.time()
  runtime <- as.numeric(end_time - start_time, units = "secs")

  status <- names(result$status)
  has_solution<-!is.null(result[["solution"]]) && is.finite(result[["objval"]])

  if (status == "TM_OPTIMAL_SOLUTION_FOUND") {
    message("Symphony found an optimal solution.")
    status<-"OPTIMAL"
  } else if (status == "TM_FEASIBLE_SOLUTION_FOUND") {
    message("Symphony found a feasible solution.")
    status<-"FEASIBLE"
  } else if (status == "TM_NO_SOLUTION") {
    message("Symphony reports the model is infeasible.")
    status<-"INFEASIBLE"
  } else if (status == "TM_UNBOUNDED") {
    message("Symphony reports the model is unbounded.")
    status<-"UNBOUNDED"
  } else if (status == "TM_TIME_LIMIT_EXCEEDED"){
    if (has_solution) {
      message("Symphone reached time limit; returning best feasible solution.")
    } else {
      message("Symphone reached time limit with no feasible solution.")
    }
    status<-"TIME_LIMIT"
  }else {
    message(paste0("Symphony returned status: ", status))
    status<-"OTHER"
  }

  if(has_solution && status %in%c("OPTIMAL","FEASIBLE","TIME_LIMIT")){
    best_solution <- result[["solution"]]
    objval <- result[["objval"]]
    idx <- hard_term_index(varname)
    x_var<-which(idx$x_index)
    auxi_var<-which(idx$obj_index)
    s_var<-which(idx$s_index)
    slack_var<-which(idx$slack_index)
    which_sol_constraints<-Matrix::rowSums(abs(A[, s_var, drop = FALSE])) > 0
    which_sol_reuse <- grepl("^(Max|Min) exposure for", rowname)
    which_sol_con<-which(which_sol_constraints & !which_sol_reuse)
    actual_LHS<-numeric(length(rhs))
    actual_LHS[!which_sol_constraints] <- as.vector(A[!which_sol_constraints,c(x_var,auxi_var),drop = FALSE] %*% best_solution[c(x_var,auxi_var)])
    actual_LHS[which_sol_reuse]<- as.vector(A[which_sol_reuse,c(x_var,s_var),drop = FALSE] %*% best_solution[c(x_var,s_var)])
    actual_LHS[which_sol_con]<-as.vector(A[which_sol_con,s_var,drop = FALSE] %*% best_solution[s_var])
    residual <- numeric(length(rhs))
    residual[sense == "<="] <- actual_LHS[sense == "<="] - rhs[sense == "<="]
    residual[sense == ">="] <- rhs[sense == ">="] - actual_LHS[sense == ">="]
    residual[sense == "="] <- actual_LHS[sense == "="] - rhs[sense == "="]
    check<-data.frame(name = rowname, LHS = actual_LHS,sense = sense, RHS = rhs,
                      residual =residual)
    solution_list<-list(numsolutions = 1,
                        objval = objval,
                        xn = best_solution)
  }else{
    best_solution<-NULL
    objval<-NA
    solution_list<-NULL
    check<-NULL
  }

  return(list(solution_found = status,
              best_solution = best_solution,
              objval = objval,
              solution_list = solution_list,
              check = check,
              runtime = runtime))
}
