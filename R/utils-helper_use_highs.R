#' @title Solve model using HiGHS
#' @description Uses HiGHS to solve a linear or MILP model for test assembly
#' @keywords  internal
use_highs<-function(A,rhs, sense, obj,lb, ub,
                     vtype,varname, modelsense,
                     params,rowname) {
  if (!requireNamespace("highs", quietly = TRUE)) {
    message("The 'highs' package is not installed.")
    return(NULL)
  }

  if(modelsense == "max"){
    maximum <- TRUE
  }else{
    maximum <-FALSE
  }

  if (!inherits(A, "dgCMatrix")) {
    A <- methods::as(A, "dgCMatrix")
  }

  num_constraints<-nrow(A)

  ## HiGHS uses "C" (continuous) and "I" (integer); no native binary
  vtype_h <- vtype
  vtype_h[vtype_h == "B"] <- "I"

  model_rhs<-rep(Inf,num_constraints)
  model_lhs<-rep(-Inf,num_constraints)
  which_equal<-which(sense=="=")
  which_le<-which(sense=="<=")
  which_ge<-which(sense==">=")
  if (length(which_equal) > 0L) {
    model_lhs[which_equal] <- rhs[which_equal]
    model_rhs[which_equal] <- rhs[which_equal]
  }
  if (length(which_le) > 0L) {
    model_rhs[which_le] <- rhs[which_le]
  }
  if (length(which_ge) > 0L) {
    model_lhs[which_ge] <- rhs[which_ge]
  }

  start_time<-Sys.time()
  result <- tryCatch({
    highs::highs_solve(L = obj,lower = lb,upper = ub,A = A,
                       rhs = model_rhs,lhs = model_lhs,
                       types = vtype_h,maximum = maximum,
                       control = params)
  }, error = function(e) {
    message("HiGHS failed: ", e$message)
    return(NULL)
  })
  end_time<-Sys.time()
  runtime <- as.numeric(end_time - start_time, units = "secs")

  status <- result$status_message
  has_solution <-  !is.null(result[["primal_solution"]]) && is.finite(result[["objective_value"]])

  if (status == "Optimal") {
    message("HiGHS found an optimal solution.")
    status<-"OPTIMAL"
  } else if (status == "Infeasible") {
    message("HiGHS reports the model is infeasible.")
    status<-"INFEASIBLE"
  } else if (status == "Unbounded"){
    message("HiGHS reports the model is unbounded.")
    status<-"UNBOUNDED"
  }else if (status == "Time limit reached"){
    status<-"TIME_LIMIT"
    if (has_solution) {
      message("HiGHS reached time limit; returning best feasible solution.")
    } else {
      message("HiGHS reached time limit with no feasible solution.")
    }
  } else if (status == "Feasible") {
    message("HiGHS found a feasible solution.")
    status <- "FEASIBLE"
  }else {
    message(paste0("HiGHS returned status: ", status))
    status<-"OTHER"
  }


  if(has_solution && status %in% c("OPTIMAL", "FEASIBLE", "TIME_LIMIT")){
    best_solution <- result[["primal_solution"]]
    objval<-result[["objective_value"]]
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
    objval<-NA_real_
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
