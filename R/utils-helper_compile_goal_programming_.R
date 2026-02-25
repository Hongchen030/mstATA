#' @title Compile a Goal Programming Objective
#'
#' @description
#' This internal helper constructs the compiled objective structure used by the
#' solver when the optimization problem follows a **goal programming** strategy.
#'
#' Goal programming minimizes deviations between achieved objective values and
#' user-specified target goals. Each objective term contributes a deviation
#' variable, and the solver minimizes a (weighted) sum of these deviations.
#'
#' This function is automatically called by \code{compile_objective()} when
#' \code{strategy = "goal_programming"}.
#'
#' @param x
#' An object of class \code{mstATA_design} created by \code{mst_design()}.
#'
#' @param obj_set A list containing multiple \code{objective_term()}
#'   object. The function will throw an error if the list does not have
#'   length greater than 1. Each term must be a *absolute* objective (i.e., with goal values).
#'
#' @param strategy A character string specifying the optimization strategy.
#' Currently, only \code{"goal_programming"} is supported.
#'
#' @param strategy_args
#' A list of strategy-specific arguments. For the \code{"goal_programming"}
#' strategy, \code{strategy_args} must be either an empty list or
#' \code{list(mode = mode, weights = weights)}. The argument \code{mode} must
#' be either \code{"one_dev"} or \code{"two_dev"}. If not provided, \code{"one_dev"}.
#' \code{weights} must be a positive numeric vector of length \code{length(obj_set)}. If not provided,
#' \code{weights} defaults to \code{rep(1, length(obj_set))}.
#'
#' @details
#'
#' **Optimization Form**
#'
#' Let there be K objective terms, each defined as a linear score
#'
#' \deqn{
#'   y_k = a_k^\top x, \quad k = 1, \ldots, K,
#' }
#'
#' where \eqn{a_k} is the coefficient vector constructed by
#' \code{objective_term()}, and x denotes the item–module–panel
#' decision variables.
#'
#' Each term has a target value \eqn{g_k}. Goal programming minimizes deviations
#' between \eqn{y_k} and \eqn{g_k}.
#' \deqn{\min \; \sum_k w_k d_k}
#'
#' where \eqn{w_k > 0} are user-specified or default weights.
#'
#'
#' Two deviation modes are supported:
#'
#' \itemize{
#'   \item \strong{One-deviation mode} (\code{mode = "one_dev"}):
#'   Introduces a single non-negative deviation variable \eqn{d_k \ge 0} per objective term such that
#'   \deqn{
#'     |a_k^\top x - g_k| \le d_k.
#'   }
#'
#'   \item \strong{Two-deviation mode} (\code{mode = "two_dev"}):
#'   Introduces separate positive and negative deviations
#'   \eqn{d_k^+ \ge 0} and \eqn{d_k^- \ge 0}, with
#'   \deqn{
#'     a_k^\top x - g_k = d_k^+ - d_k^-.
#'   }
#' }
#'
#'
#' **Interpretation**
#' The compiled objective minimizes a weighted sum of deviations:
#'
#' \deqn{
#'   \min \sum_{k=1}^K w_k \, d_k
#' }
#'
#' in one-deviation mode, or
#'
#' \deqn{
#'   \min \sum_{k=1}^K w_k \, (d_k^+ + d_k^-)
#' }
#'
#' in two-deviation mode, where \eqn{w_k} are user-specified or default weights.
#'
#' All goal programming objectives are compiled as **minimization** problems.
#'
#' @return A list of class \code{"compiled_objective"}
#' \describe{
#'   \item{name}{A character vector indicating the specifications in each row of `A_binary`}
#'   \item{specification}{A \code{data.frame} summarizing the constraint specification, including
#'    the requirement name, attribute, constraint type, application level,
#'    operator, and the number of constraint rows generated.}
#'   \item{A_binary}{Sparse matrix of coefficients for binary decision variables.}
#'   \item{A_real}{Sparse matrix of coefficients for real decision variables.}
#'   \item{operators}{A character vector of constraint operators, one per row of `A_binary`.}
#'   \item{d}{A numeric vector of right-hand-side values for the constraints.}
#'   \item{C_binary}{Penalty vector for binary variables (if any)}
#'   \item{C_real}{Penalty vector for real variables (if any)}
#'   \item{sense}{Always \code{"min"} for the goal_programming strategy.}
#'   \item{decisionvar_name_new}{Character vector indicating the names of new decision variables, same length as `ncol(A_real)`.}
#'   \item{decisionvar_type_new}{A character vector of `"C"` (continuous), same length as `ncol(A_real)`.}
#'   \item{notes}{A list containing the strategy name, strategy arguments, and \code{y_bounds}.}
#' }
#' @keywords internal
#'

compile_goal_programming_ <- function(x, obj_set,strategy = "goal_programming",
                                      strategy_args = list()) {
  if (inherits(obj_set, "mstATA_objective")) {
    obj_set <- list(obj_set)
  } else if (is.list(obj_set)) {
    if (length(obj_set) == 0L) {
      stop("'obj_set' must contain at least one 'mstATA_objective'.", call. = FALSE)
    }
    is_obj <- vapply(obj_set,inherits,what = "mstATA_objective",FUN.VALUE = logical(1L))
    if (!all(is_obj)) {
      stop("'obj_set' must all be 'mstATA_objective' objects.", call. = FALSE)
    }
  } else {
    stop("'obj_set' must be a 'mstATA_objective' or a list of them.", call. = FALSE)
  }
  strategy <- match.arg(strategy,"goal_programming")
  num_objs <- length(obj_set)
  if (num_objs == 1L) {
    stop("Multiple-term strategies require at least two terms. Use 'single' instead.")
  }

  term_names <- vapply(obj_set, function(t) t$name,character(1L))
  term_goals <- vapply(obj_set,function(t) {
    if (is.null(t$goal)) NA_real_ else as.numeric(t$goal)
  },numeric(1L))
  if (anyDuplicated(term_names)) stop("All objective term names must be unique.")

  terms_senses <- vapply(obj_set, function(t) t$sense, character(1L))
  if(any(terms_senses == "max")){
    stop(
      "goal_programming requires all objective terms to have sense = 'min'.",
      call. = FALSE
    )
  }
  strategy_args <- validate_strategy_args(strategy, strategy_args, n_terms = num_objs,
                                          goals = term_goals)

  mode<-strategy_args$mode
  weight_vals<-strategy_args$weights

  decisionvar_name<-x$decisionvar_name
  n_bin<-length(decisionvar_name)

  A_list  <- lapply(obj_set, `[[`, "coef_val")
  goal_list  <- lapply(obj_set, `[[`, "goal")

  n_rows <- 2L*num_objs
  nnz_bin <- sum(vapply(A_list, function(a) sum(a != 0), integer(1)) * 2L)

  if(mode == "one_dev"){
    new_var_name<-paste0("d",seq_len(num_objs),"(abs_dev)")
    new_var_type<-rep("C",num_objs)
    C_real<-weight_vals
    y_bounds<-matrix(0,nrow = num_objs,ncol = 2)
  }else{
    new_var_name <- as.vector(rbind(paste0("d", seq_len(num_objs), "(shortfall)"),
                                    paste0("d", seq_len(num_objs), "(excess)")))
    new_var_type<-rep("C",num_objs*2)
    C_real<-rep(weight_vals,each=2L)
    y_bounds<-matrix(0,nrow = num_objs*2,ncol = 2)
  }

  rows_i_b <- integer(nnz_bin)
  rows_j_b <- integer(nnz_bin)
  rows_x_b <- numeric(nnz_bin)

  rows_i_r <- integer(n_rows)
  rows_j_r <- integer(n_rows)
  rows_x_r <- numeric(n_rows)

  operators <- character(n_rows)
  rhs       <- numeric(n_rows)
  ConstraintMatrix_name<-character(n_rows)

  row_ptr<-1L
  bin_ptr<-1L
  for(obj_id in seq_len(num_objs)){
    a_vec<-as.vector(A_list[[obj_id]])
    nz_b<-which(a_vec!=0)
    bounds<-estimate_term_bounds(x = x,a_vec = a_vec)
    goal<-goal_list[[obj_id]]
    rows_i_b[bin_ptr:(bin_ptr+2*length(nz_b)-1)]<-rep(c(row_ptr,row_ptr+1L),each=length(nz_b))
    rows_j_b[bin_ptr:(bin_ptr+2*length(nz_b)-1)]<-rep(nz_b,2)
    rows_x_b[bin_ptr:(bin_ptr+2*length(nz_b)-1)]<-rep(a_vec[nz_b],2)
    lb<-0
    ub<-max(abs(bounds[["lower_bound"]]-goal),abs(bounds[["upper_bound"]]-goal))
    if(mode == "one_dev"){
      rows_i_r[row_ptr:(row_ptr+1L)]<-c(row_ptr,row_ptr+1L)
      rows_j_r[row_ptr:(row_ptr+1L)]<-rep(obj_id,2)
      rows_x_r[row_ptr:(row_ptr+1L)]<-c(1L,-1L)
      y_bounds[obj_id,]<-c(lb,ub)
    }else{
      rows_i_r[row_ptr:(row_ptr+1L)]<-c(row_ptr,row_ptr+1L)
      rows_j_r[row_ptr:(row_ptr+1L)]<-(2*obj_id-1):(2*obj_id)
      rows_x_r[row_ptr:(row_ptr+1L)]<-c(1L,-1L)
      y_bounds[(2*obj_id-1),]<-c(lb,ub)
      y_bounds[(2*obj_id),]<-c(lb,ub)
    }
    operators[row_ptr:(row_ptr+1L)]<-c(">=","<=")
    rhs[row_ptr:(row_ptr+1L)]<-c(goal,goal)
    ConstraintMatrix_name[row_ptr:(row_ptr+1L)]<-c(paste0(term_names[obj_id],"(shortfall)"),
                                                   paste0(term_names[obj_id],"(excess)"))

    bin_ptr<-bin_ptr+2*length(nz_b)
    row_ptr<-row_ptr+2L
  }

  A_binary <- Matrix::sparseMatrix(i = rows_i_b, j = rows_j_b, x = rows_x_b,
                                   dims = c(n_rows, n_bin))
  colnames(A_binary)<-colnames(A_list[[1]])

  A_real <- Matrix::sparseMatrix(i = rows_i_r, j = rows_j_r, x = rows_x_r,
                                 dims = c(n_rows, length(new_var_name)))
  colnames(A_real)<-new_var_name

  Specification <- do.call(
    rbind,
    lapply(seq_len(num_objs), function(k) {
      parsed <- parse_objective_string(term_names[k])
      if(parsed$attribute_type == "Category"){
        spec<-paste0("Item count from ",parsed$attribute)
      }else{
        spec<-paste0("Sum of ",parsed$attribute)
      }
      data.frame(
        Requirement = spec,
        Attribute = parsed$attribute,
        Type = parsed$attribute_type,
        `Application Level` = parsed$application_level,
        Operator = parsed$objective_type,
        `Num of Constraints` = 2L,
        stringsAsFactors = FALSE
      )
    })
  )
  colnames(Specification)<-c("Requirement","Attribute","Type","Application Level","Operator","Num of Constraints")
  Specification$`Num of Constraints`<-as.numeric(Specification$`Num of Constraints`)

  return(create_objective(name = ConstraintMatrix_name,
                          specification = Specification,
                          A_binary = A_binary,
                          A_real   = A_real,
                          C_binary = NULL,
                          C_real   = C_real,
                          operator = operators,
                          d      = rhs,
                          sense = "min",
                          decisionvar_name_new = new_var_name,
                          decisionvar_type_new    = new_var_type,
                          notes  = list(strategy = strategy,
                                        strategy_args = strategy_args,
                                        y_bounds = y_bounds)))
}
