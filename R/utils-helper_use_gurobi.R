#' @title Solve model using gurobi
#' @description Uses gurobi to solve a linear or MILP model for test assembly
#' @keywords  internal
use_gurobi<-function(A,rhs, sense, obj,lb, ub,
                     vtype,varname, modelsense,
                     params,rowname) {

  # Check installation without requireNamespace()
  if (!("gurobi" %in% rownames(utils::installed.packages()))){
    stop("The 'gurobi' package is not installed.",call. = FALSE)
  }
  # Dynamically access namespace (no gurobi::)
  ns <- getNamespace("gurobi")
  gurobi_fun <- get("gurobi", envir = ns)
  gurobi_iis_fun <- get("gurobi_iis", envir = ns)

  if (!inherits(A, "sparseMatrix")) A <- Matrix::Matrix(A, sparse = TRUE)

  model <- list(A = A,rhs = rhs,sense = sense,obj = obj,lb = lb, ub = ub,
                vtype = vtype,varname = varname,modelsense = modelsense)
  params <- params
  start_time<-Sys.time()
  result <- tryCatch({
    gurobi_fun(model = model, params = params)
  }, error = function(e) {
    message("gurobi failed: ", e$message)
    return(NULL)
  })
  end_time<-Sys.time()
  runtime <- as.numeric(end_time - start_time, units = "secs")


  has_solution <- !is.null(result$x) && is.finite(result$objval)
  raw_status <- result$status
  status <- raw_status
  IIS <- NULL

  if (raw_status == "OPTIMAL") {
    message("gurobi found an optimal solution.")
  } else if (raw_status == "SUBOPTIMAL") {
    status<-"FEASIBLE"
    message("gurobi found a feasible solution.")
  } else if (raw_status == "INFEASIBLE") {
    message("gurobi reports the model is infeasible.")
    IIS <- tryCatch({
      find_iis <- gurobi_iis_fun(model = model, params = params)
      rowname[which(find_iis$Arows)]
    }, error = function(e) {
      warning("Failed to compute IIS: ", e$message)
      NULL
    })
  } else if (raw_status == "UNBOUNDED") {
    message("gurobi reports the model is unbounded.")
  } else if (raw_status == "TIME_LIMIT") {
    if (has_solution) {
      message("Gurobi reached time limit; returning best feasible solution.")
    } else {
      message("Gurobi reached time limit with no feasible solution.")
    }
  } else {
    message(paste0("gurobi returned status: ", status))
    status<-"OTHER"
  }


  if (has_solution && status %in% c("OPTIMAL", "FEASIBLE", "TIME_LIMIT")) {
    best_solution <- result$x
    solution_list <-result$pool
    objval <- result$objval
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
              runtime = runtime,
              iis = IIS))
}
