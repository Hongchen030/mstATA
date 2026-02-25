#' @title Compile a Minimax Objective
#'
#' @description
#'
#' Constructs the compiled objective structure for the **minimax** strategy,
#' which minimizes the *worst-case deviation* from target values across
#' multiple objective terms.
#'
#' The minimax strategy seeks a solution that balances objectives by controlling
#' the largest absolute mismatch between each linear score \eqn{a_k^\top x}
#' and its target value \eqn{g_k}.
#'
#' This function is automatically invoked by \code{compile_objective()} when
#' \code{strategy = "minimax"}.
#'
#' @param x
#' An object of class \code{mstATA_design} created by \code{mst_design()}.
#'
#' @param obj_set A list containing multiple \code{objective_term()}
#'   object. The function will throw an error if the list does not have
#'   length greater than 1. Each term must be a *absolute* objective (i.e., with goal values).
#'
#' @param strategy A character string specifying the optimization strategy.
#' Currently, only \code{"minimax"} is supported.
#'
#' @param strategy_args
#' A list of strategy-specific arguments. For the \code{"minimax"}
#' strategy, \code{strategy_args} must be either an empty list or
#' \code{list(mode = mode)}. The argument \code{mode} must
#' be either \code{"one_dev"} or \code{"two_dev"}. If not provided, \code{"one_dev"}.
#'
#' @details
#'
#' **1. Overview**
#'
#' Let there be \eqn{K} objective terms, each defined as a linear score
#'
#' \deqn{
#'   y_k = a_k^\top x, \quad k = 1, \ldots, K,
#' }
#'
#' where \eqn{a_k} is the coefficient vector constructed by
#' \code{objective_term()}, and \eqn{x} denotes the vector of
#' item–module–panel decision variables.
#'
#' Each objective term has a user-specified target value \eqn{g_k}.
#'
#' The minimax strategy minimizes the maximum deviation from these targets,
#' producing a solution that controls the worst-performing objective.
#'
#' Two deviation modes are supported:
#'
#' \itemize{
#'   \item \strong{One-deviation mode} (\code{mode = "one_dev"}):
#'   Introduces a common non-negative deviation variable \eqn{d \ge 0} such that
#'   \deqn{
#'     |a_k^\top x - g_k| \le d.
#'   }
#'
#'   \item \strong{Two-deviation mode} (\code{mode = "two_dev"}):
#'   Introduces separate positive and negative deviations
#'   \eqn{d^+ \ge 0} and \eqn{d^- \ge 0}, with
#'   \deqn{
#'     a_k^\top x - g_k = d^+ - d^-.
#'   }
#' }
#'
#'
#' **2. Optimization Form**
#'
#' Introduce one/two continuous auxiliary variable \eqn{d} or \eqn{d^+,d^-} representing the
#' **maximum deviation** across all objective terms. The optimization problem is:
#'
#' \deqn{
#'   \min \; d
#' }
#'
#' or
#'
#' \deqn{
#'   \min \; d^+ + d^-
#' }
#'
#' subject to:
#'
#' **one_dev**
#'
#' \deqn{
#'   |a_k^\top x - g_k| \le d, \quad k = 1, \ldots, K.
#' }
#'
#' **two_dev**
#'
#' \deqn{
#'   a_k^\top x + d^- \le  g_k , \quad k = 1, \ldots, K.
#'
#'   a_k^\top x -d^+  \le g_k, \quad k = 1, \ldots, K.
#' }
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
#'   \item{sense}{Always \code{"min"} for the minimax strategy.}
#'   \item{decisionvar_name_new}{Character vector indicating the names of new decision variables, same length as `ncol(A_real)`.}
#'   \item{decisionvar_type_new}{A character vector of `"C"` (continuous), same length as `ncol(A_real)`.}
#'   \item{notes}{A list containing the strategy name, strategy arguments, and \code{y_bounds}.}
#' }
#' @keywords internal
#'
compile_minimax_ <- function(x, obj_set,strategy = "minimax",
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
  strategy <- match.arg(strategy,"minimax")
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
      "minimax requires all objective terms to have sense = 'min'.",
      call. = FALSE
    )
  }

  strategy_args <- validate_strategy_args(strategy, strategy_args, n_terms = num_objs,
                                          goals = term_goals)

  mode<-strategy_args$mode

  decisionvar_name<-x$decisionvar_name
  n_bin<-length(decisionvar_name)

  A_list  <- lapply(obj_set, `[[`, "coef_val")

  term_bounds <- lapply(A_list, function(a) estimate_term_bounds(x = x, a_vec = a))
  lb_vec   <- vapply(term_bounds, `[[`, numeric(1), "lower_bound")
  ub_vec   <- vapply(term_bounds, `[[`, numeric(1), "upper_bound")

  if(mode == "one_dev"){
    new_var_name<-"d(abs_dev)"
    new_var_type<-"C"
    C_real<-1L
    ub_d <- max(pmax(abs(lb_vec - term_goals), abs(ub_vec - term_goals), 0))
    y_bounds <- matrix(c(0, ub_d), nrow = 1)
  }else{
    new_var_name<-c("d(shortfall)","d(excess)")
    new_var_type<-c("C","C")
    C_real<-c(1L,1L)
    ub_short <- max(pmax(term_goals - lb_vec, 0))
    ub_excess <- max(pmax(ub_vec - term_goals, 0))
    y_bounds <- rbind(c(0, ub_short), c(0, ub_excess))
  }

  n_rows<-2L*num_objs
  nnz_bin <- sum(vapply(A_list, function(a) sum(a != 0), integer(1)) * 2L)

  rows_i_b <- integer(nnz_bin)
  rows_j_b <- integer(nnz_bin)
  rows_x_b <- numeric(nnz_bin)

  rows_i_r <- integer(n_rows)
  rows_j_r <- integer(n_rows)
  rows_x_r <- numeric(n_rows)

  operators <- rep(c(">=","<="),num_objs)
  rhs       <- rep(term_goals,each = 2L)
  ConstraintMatrix_name<-as.vector(rbind(paste0(term_names, "(shortfall)"),
                                         paste0(term_names, "(excess)")))

  row_ptr<-1L
  bin_ptr<-1L
  for(obj_id in seq_len(num_objs)){
    a_vec<-as.vector(A_list[[obj_id]])
    nz_b<-which(a_vec!=0)
    if(length(nz_b)>0L){
      idx <- bin_ptr:(bin_ptr + 2L * length(nz_b) - 1L)
      rows_i_b[idx] <- rep(c(row_ptr, row_ptr + 1L), each = length(nz_b))
      rows_j_b[idx] <- rep(nz_b, 2L)
      rows_x_b[idx] <- rep(a_vec[nz_b], 2L)
      bin_ptr <- bin_ptr + 2L * length(nz_b)
    }

    if(mode == "one_dev"){
      rows_i_r[row_ptr:(row_ptr+1L)]<-c(row_ptr,row_ptr+1L)
      rows_j_r[row_ptr:(row_ptr+1L)]<-c(1L,1L)
      rows_x_r[row_ptr:(row_ptr+1L)]<-c(1L,-1L)
    }else{
      rows_i_r[row_ptr:(row_ptr+1L)]<-c(row_ptr,row_ptr+1L)
      rows_j_r[row_ptr:(row_ptr+1L)]<-c(1L,2L)
      rows_x_r[row_ptr:(row_ptr+1L)]<-c(1L,-1L)
    }
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
