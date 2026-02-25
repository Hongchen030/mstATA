#' @title Solve model using GLPK
#' @description Uses GLPK to solve a linear or MILP model for test assembly
#' @keywords  internal
use_glpk<-function(A,rhs, sense, obj,lb, ub,
                     vtype,varname, modelsense,
                     params,rowname) {
  if (!requireNamespace("Rglpk", quietly = TRUE)) {
    message("The 'Rglpk' package is required. Install it using install.packages('Rglpk')")
    return(NULL)
  }

  if (!inherits(A, "sparseMatrix")) A <- Matrix::Matrix(A, sparse = TRUE)

  if(modelsense == "max"){
    max<-TRUE
  }else{
    max<-FALSE
  }

  which_equal<-which(sense=="=")
  sense[which_equal]<-"=="
  start_time<-Sys.time()
  result <- tryCatch({
    Rglpk::Rglpk_solve_LP(obj = obj,mat = A,dir = sense,rhs = rhs,
                          bounds = list(lb = lb, ub = ub),types = vtype,
                          max = max,control = params)
  }, error = function(e) {
    message("GLPK MIP failed: ", e$message)
    return(NULL)
  })
  end_time<-Sys.time()
  runtime <- as.numeric(end_time - start_time, units = "secs")

  has_solution<-!is.null(result$solution) && is.finite(result$optimum)
  raw_status<-result$status
  if(raw_status==4){
    message("GLPK reports the model is infeasible.")
    status<-"INFEASIBLE"
  }else if (raw_status==5) {
    message("GLPK found an optimal solution.")
    status <- "OPTIMAL"
  } else if (raw_status==2) {
    message("GLPK found a feasible solution.")
    status<-"FEASIBLE"
  } else if (raw_status==6) {
    message("GLPK reports the model is unbounded.")
    status<-"UNBOUNDED"
  } else if (runtime >= (params$tm_limit - 0.05) & raw_status==3){
    status<-"TIME_LIMIT"
    if (has_solution) {
      message("GLPK reached time limit; returning best feasible solution.")
    } else {
      message("GLPK reached time limit with no feasible solution.")
    }
  }else{
    status<-"OTHER"
    message(paste0("GLPK returned status: ", status))
  }


  if(status%in%c("OPTIMAL","FEASIBLE","TIME_LIMIT")){
    objval<-result$optimum
    best_solution <- result$solution
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
    solution_list <- list(numsolutions = 1,
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
