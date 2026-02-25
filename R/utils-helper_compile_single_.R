#' @title Compile a Single Linear Objective Term
#'
#' @description
#' This internal helper constructs the compiled objective structure used by
#' the solver when the optimization problem contains **exactly one**
#' objective term.
#'
#' In the single-objective case, the optimization reduces to maximizing or
#' minimizing a single linear score:
#'
#' \deqn{
#'   \max \; y \quad \text{or} \quad \min \; y,
#' }
#'
#' where \eqn{y = a^\top x} is the linear objective term created by
#' \code{objective_term()}.
#'
#' This function is automatically called by \code{compile_objective()} when
#' \code{strategy = "single"} in \code{objective_set()}.
#'
#' @param x An object of class \code{mstATA_design} created by \code{mst_design()}.
#'
#' @param obj_set A list containing exactly one \code{objective_term()}
#'   object. The function will throw an error if the list does not have
#'   length 1.
#'
#' @param strategy A character string specifying the optimization strategy.
#' Currently, only \code{"single"} is supported.
#'
#' @param strategy_args A list of strategy-specific arguments. For the \code{"single"} strategy,
#' no additional arguments are supported; therefore, \code{strategy_args}
#' must be an empty list (\code{list()}).
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
#'   \item{sense}{"min" or "max"}
#'   \item{decisionvar_name_new}{Character vector indicating the names of new decision variables, same length as `ncol(A_real)`.}
#'   \item{decisionvar_type_new}{A character vector of `"C"` (continuous), same length as `ncol(A_real)`.}
#'   \item{notes}{A list containing the strategy name, strategy arguments, and \code{y_bounds}.}
#' }
#'
#' @keywords internal
#'
compile_single_ <- function(x, obj_set,strategy = "single",
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

  strategy <- match.arg(strategy,"single")
  n_terms <- length(obj_set)
  if (n_terms != 1L) {
    stop("strategy='single' requires exactly one term.")
  }
  if (!is.list(strategy_args) || length(strategy_args) != 0L) {
    stop(
      "`strategy_args` must be an empty list (use `list()`).",
      call. = FALSE
    )
  }

  terms <- obj_set[[1]]
  sense <- terms$sense
  name<-terms$name
  a_vec<-terms$coef_val
  new_var_name<-paste("y",sense,"value")
  new_var_type<-"C"
  a_vec<-as.vector(a_vec)
  temp_bounds <- estimate_term_bounds(x = x, a_vec = a_vec)
  if(is.null(terms$goal)){
    row_need<-1
    A_real<- -1
    rhs<-0
    ConstraintMatrix_name<-name
    y_bounds <- matrix(temp_bounds,nrow = 1)
  }else{
    row_need<-2
    A_real<-c(1,-1)
    target<-terms$goal
    rhs<-c(target,target)
    ConstraintMatrix_name<-c(paste0(name,"(shortfall)"),
                             paste0(name,"(excess)"))
    y_bounds <- matrix(c(0,max(abs(temp_bounds[["lower_bound"]]-target),abs(temp_bounds[["upper_bound"]]-target))),
                       nrow = 1)
  }

  decisionvar_name<-x$decisionvar_name
  decisionvar_type<-rep("B",length(decisionvar_name))
  n_bin<-length(decisionvar_name)


  # Collect triplets for A_binary and A_real
  nz_b <- which(a_vec != 0)
  rows_i_b <- rep.int(seq_len(row_need),length(nz_b))
  rows_j_b <- rep(nz_b,row_need)
  rows_x_b <- rep(a_vec[nz_b],row_need)
  rows_i_r <- seq_len(row_need)
  rows_j_r <- rep(1L,row_need)
  rows_x_r <- A_real
  if(row_need==1){
    if(sense=="max"){
      operator<-">="
    }else{
      operator<-"<="
    }
  }else{
    operator<-c(">=","<=")
  }


  A_binary<-Matrix::sparseMatrix(i = rows_i_b, j = rows_j_b, x = rows_x_b,
                                 dims = c(row_need, n_bin))
  colnames(A_binary)<-colnames(a_vec)
  A_real <- Matrix::sparseMatrix(i = rows_i_r, j = rows_j_r, x = rows_x_r,
                                 dims = c(row_need, 1L))
  colnames(A_real)<-new_var_name
  obj_name<-as.vector(parse_objective_string(name))
  parsed <- parse_objective_string(name)
  if(parsed$attribute_type == "Category"){
    spec<-paste0("Item count from ",parsed$attribute)
  }else{
    spec<-paste0("Sum of ",parsed$attribute)
  }
  Specification<-data.frame(a=spec,b=obj_name$attribute,c=obj_name$attribute_type,d=obj_name$application_level,
                            e=obj_name$objective_type,f=row_need,
                            stringsAsFactors = FALSE)
  colnames(Specification)<-c("Requirement","Attribute","Type","Application Level","Operator","Num of Constraints")
  Specification$`Num of Constraints`<-as.numeric(Specification$`Num of Constraints`)

  return(create_objective(name = ConstraintMatrix_name,specification = Specification,
                          A_binary = A_binary,
                          A_real   = A_real,
                          C_binary = NULL,
                          C_real   = 1L,
                          operator = operator,
                          d      = rhs,
                          sense = sense,
                          decisionvar_name_new = new_var_name,
                          decisionvar_type_new    = new_var_type,
                          notes  = list(strategy = strategy,
                                        strategy_args = list(sense = sense),
                                        y_bounds = y_bounds)))
}
