#' @title Solve model using lpSolveAPI
#' @description Uses lpSolveAPI to solve a linear or MILP model for test assembly
#' @keywords  internal
use_lpsolve<-function(A,rhs, sense, obj,lb, ub,
                      vtype,varname, modelsense,
                      params,rowname) {

  if (!requireNamespace("lpSolveAPI", quietly = TRUE)) {
    message("The 'lpSolveAPI' package is required. Install it using install.packages('lpSolveAPI')")
    return(NULL)
  }

  params<-params
  presolve<- params[["presolve"]]
  timeout<-params[["timeout"]]
  mip.gap <- params[["mip_gap"]]
  epsb<-params[["epsb"]]
  epsd<-params[["epsd"]]

  num_decisions<-ncol(A)
  L<-lpSolveAPI::make.lp(0,num_decisions)
  lpSolveAPI::set.objfn(L,obj = obj)

  which_binary<-which(vtype=="B")
  which_real<-which(vtype=="C")
  which_integer <- which(vtype == "I")

  lpSolveAPI::set.type(L,columns = which_binary,type = "binary")
  lpSolveAPI::set.type(L,columns = which_real,type = "real")
  lpSolveAPI::set.type(L, columns = which_integer, type = "integer")
  lpSolveAPI::set.bounds(L,lower = lb,upper = ub)

  if (!inherits(A, "sparseMatrix")) A <- Matrix::Matrix(A, sparse = TRUE)
  S<-Matrix::summary(A)
  i_idx<-split(seq_len(nrow(S)),S$i)
  for(constraint_id in names(i_idx)){
    r<-as.integer(constraint_id)
    ix<-i_idx[[constraint_id]]
    lpSolveAPI::add.constraint(L,
                   xt = S$x[ix],type = sense[r],
                   rhs = rhs[r],indices = S$j[ix])
  }

  lpSolveAPI::lp.control(L,presolve = presolve,timeout = timeout,mip.gap = mip.gap,
             epsb = epsb,epsd = epsd,sense = modelsense)
  start_time<-Sys.time()
  result <- tryCatch({
    lpSolveAPI::solve.lpExtPtr(L)
  }, error = function(e) {
    message("lpsolve failed: ", e$message)
    return(NULL)
  })
  end_time<-Sys.time()
  runtime <- as.numeric(end_time - start_time, units = "secs")

  map <- c(`0`  = "OPTIMAL",
           `1`  = "SUBOPTIMAL",
           `2`  = "INFEASIBLE",
           `3`  = "UNBOUNDED",
           `4`  = "DEGENERATE",
           `5`  = "NUMFAILURE",
           `6`  = "USERABORT",
           `7`  = "TIMEOUT",
           `9`  = "PRESOLVED",
           `10` = "PROCFAIL",
           `11` = "PROCBREAK",
           `12` = "FEASFOUND",
           `13` = "NOFEASFOUND")
  status <- unname(map[as.character(result)])

  has_solution<-TRUE
  if (status == "OPTIMAL") {
    message("lpsolve found an optimal solution.")
  } else if (status == "SUBOPTIMAL") {
    message("lpsolve found a feasible solution.")
    status<-"FEASIBLE"
  } else if (status == "INFEASIBLE") {
    message("lpsolve reports the model is infeasible.")
  } else if (status == "UNBOUNDED") {
    message("lpsolve reports the model is unbounded.")
  } else if(status == "TIMEOUT"){
    has_solution<-!is.null(lpSolveAPI::get.variables(L)) && is.finite(lpSolveAPI::get.objective(L))
    if (has_solution) {
      message("lpsolve reached time limit; returning best feasible solution.")
    } else {
      message("lpsolve reached time limit with no feasible solution.")
    }
    status<-"TIME_LIMIT"
  }else{
    message(paste0("lpsolve returned status: ", status))
    status<-"OTHER"
  }


  if(has_solution && status%in%c("OPTIMAL","FEASIBLE","TIME_LIMIT")){
    objval<-lpSolveAPI::get.objective(L)
    best_solution<-lpSolveAPI::get.variables(L)
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
    solution_list<-list(numsolutions = lpSolveAPI::get.solutioncount(L),
                        objval = objval,
                        xn = best_solution)
  }else{
    best_solution<-NULL
    objval<-NA_real_
    solution_list <-NULL
    check<-NULL
  }


  return(list(solution_found = status,
              best_solution = best_solution,
              objval = objval,
              solution_list = solution_list,
              check = check,
              runtime = runtime))
}
