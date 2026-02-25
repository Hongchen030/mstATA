#' @title Compile a Capped-Maximin Objective
#'
#' @description
#' Implements the **capped-maximin** objective aggregation strategy, which seeks to
#' maximize a common lower bound across multiple objective terms while penalizing
#' excessive deviation above that bound.
#'
#' The capped-maximin strategy balances multiple competing objectives by maximizing
#' the worst-performing (scaled) objective, while allowing limited flexibility
#' through an overflow parameter.
#'
#' This function is automatically called by \code{compile_objective()} when
#' \code{strategy = "capped_maximin"}.
#'
#' @param x
#' An object of class \code{mstATA_design} created by \code{mst_design()}.
#'
#' @param obj_set A list containing multiple \code{objective_term()}
#'   object. The function will throw an error if the list does not have
#'   length greater than 1. Each term must be a *relative* objective (i.e., no goal values).
#'
#' @param strategy A character string specifying the optimization strategy.
#' Currently, only \code{"capped_maximin"} is supported.
#'
#' @param strategy_args
#' A list of strategy-specific arguments. For the \code{"capped_maximin"}
#' strategy, \code{strategy_args} must be either an empty list or
#' \code{list(proportions = proportions)}, where \code{proportions} is a positive
#' numeric vector of length \code{length(obj_set)}. If omitted, \code{proportions}
#' defaults to \code{rep(1, length(obj_set))}.
#'
#' @details
#'
#' ## Optimization Formulation
#'
#' Let there be \eqn{K} objective terms, each defined by a linear score:
#'
#' \deqn{
#'   y_k = a_k^\top x, \quad k = 1, \ldots, K,
#' }
#'
#' where:
#'
#' \itemize{
#'   \item \eqn{a_k} is the coefficient vector constructed by
#'   \code{objective_term()};
#'   \item \eqn{x} denotes the binary item–module–panel decision variables;
#'   \item \eqn{p_k > 0} is a user-specified proportions that scales the relative
#'   importance of objective \eqn{k}.
#' }
#'
#' The capped-maximin strategy introduces two auxiliary continuous variables:
#'
#' \itemize{
#'   \item \eqn{y}, representing a common lower bound on the scaled objective values;
#'   \item \eqn{\delta}, representing the maximum allowed overflow above this bound.
#' }
#'
#' The optimization problem is:
#'
#' \deqn{
#'   \max \; y - \delta
#' }
#'
#' subject to the constraints:
#'
#' \deqn{
#'   p_k\, y \;\le\; a_k^\top x \;\le\; p_k\, y + \delta,
#'   \quad k = 1, \ldots, K.
#' }
#'
#' This formulation ensures that all objective scores are kept close to a common
#' proportional baseline, while allowing controlled deviation when perfect balance
#' is infeasible.
#'
#' ## Interpretation
#'
#' \itemize{
#'   \item The variable \eqn{y} captures the *worst-case* normalized objective value
#'   across all terms.
#'
#'   \item The variable \eqn{\delta} controls how much any objective is allowed to
#'   exceed its proportional share.
#'
#'   \item Maximizing \eqn{y - \delta} simultaneously encourages fairness across
#'   objectives and limits excessive dominance by any single objective.
#' }
#'
#' Compared to standard maximin, capped-maximin provides greater numerical stability
#' and flexibility, especially when objective terms differ substantially in scale.
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
#'   \item{C_binary}{A numeric vector of objective coefficients for binary decision
#'   variables.}
#'   \item{C_real}{A numeric vector of objective coefficients for continuous
#'   decision variables.}
#'   \item{sense}{Always \code{"max"} for the capped_maximin strategy.}
#'   \item{decisionvar_name_new}{Character vector indicating the names of new decision variables, same length as `ncol(A_real)`.}
#'   \item{decisionvar_type_new}{A character vector of `"C"` (continuous), same length as `ncol(A_real)`.}
#'   \item{notes}{A list containing the strategy name, strategy arguments, and \code{y_bounds}.}
#' }
#' @keywords internal

compile_capped_maximin_ <- function(x, obj_set,strategy = "capped_maximin",
                                    strategy_args = list()) {
  if (inherits(obj_set, "mstATA_objective")) {
    obj_set <- list(obj_set)
  } else if (is.list(obj_set)) {
    if (length(obj_set) <=1L) {
      stop("'obj_set' must contain at least one 'mstATA_objective'.", call. = FALSE)
    }
    is_obj <- vapply(obj_set,inherits,what = "mstATA_objective",FUN.VALUE = logical(1L))
    if (!all(is_obj)) {
      stop("'obj_set' must all be 'mstATA_objective' objects.", call. = FALSE)
    }
  } else {
    stop("'obj_set' must be a 'mstATA_objective' or a list of them.", call. = FALSE)
  }

  strategy <- match.arg(strategy,"capped_maximin")
  num_objs <- length(obj_set)
  if (num_objs == 1L) {
    stop("Multiple-term strategies require at least two terms. Use 'single_obj()' instead.")
  }

  term_names <- vapply(obj_set, function(t) t$name,character(1L))
  term_goals <- vapply(obj_set,function(t) {
    if (is.null(t$goal)) NA_real_ else as.numeric(t$goal)
  },numeric(1L))
  if (anyDuplicated(term_names)) stop("All objective term names must be unique.")

  terms_senses <- vapply(obj_set, function(t) t$sense, character(1L))
  if (any(terms_senses == "min")) {
    stop(
      "capped_maximin requires all objective terms to have sense = 'max'.",
      call. = FALSE
    )
  }

  strategy_args <- validate_strategy_args(strategy, strategy_args, n_terms = num_objs,
                                          goals = term_goals)
  proportions <- strategy_args$proportions

  decisionvar_name<-x$decisionvar_name
  n_bin   <- length(decisionvar_name)

  A_list  <- lapply(obj_set, `[[`, "coef_val")
  new_var_name<-c("y_capped_maximin","delta")
  new_var_type<-c("C","C")

  term_bounds <- lapply(A_list, function(a) estimate_term_bounds(x = x, a_vec = a))
  lb   <- vapply(term_bounds, `[[`, numeric(1), "lower_bound")
  ub   <- vapply(term_bounds, `[[`, numeric(1), "upper_bound")
  lo <- max(lb / proportions)
  hi <- min(ub / proportions)
  if (!is.finite(lo)) lo <- -Inf
  if (!is.finite(hi)) hi <-  Inf
  if (is.finite(lo) && is.finite(hi) && lo > hi) {
    # infeasible overlap; keep wide bounds rather than invalid interval
    lo <- -Inf
    hi <-  Inf
  }
  y_bounds <- matrix(c(lo, hi,0,hi), nrow = 2,ncol = 2,byrow = TRUE)

  n_rows <- num_objs * 2L
  nnz_bin <- sum(vapply(A_list, function(a) sum(a != 0), integer(1)) * 2L)

  rows_i_b <- integer(nnz_bin)
  rows_j_b <- integer(nnz_bin)
  rows_x_b <- numeric(nnz_bin)

  operators <- rep(c(">=","<="),num_objs)
  rhs       <- rep(0,n_rows)
  ConstraintMatrix_name<-character(n_rows)


  A_real <- Matrix::Matrix(0, nrow = n_rows, ncol = 2, sparse = TRUE)
  colnames(A_real)<-new_var_name

  row_ptr <- 1L
  bin_ptr <- 1L
  for (obj_id in seq_len(num_objs)) {
    a_vec <- as.vector(A_list[[obj_id]])
    nz_b   <- which(a_vec != 0)
    p_k<-proportions[obj_id]
    if(length(nz_b)>0){
      idx <- bin_ptr:(bin_ptr + 2L*length(nz_b) - 1L)
      rows_i_b[idx] <- rep(c(row_ptr, row_ptr+1L), each = length(nz_b))
      rows_j_b[idx] <- rep(nz_b, 2L)
      rows_x_b[idx] <- rep(a_vec[nz_b], 2L)
      bin_ptr <- bin_ptr + 2L*length(nz_b)
    }
    A_real[row_ptr:(row_ptr+1L),1]<- -p_k
    A_real[row_ptr:(row_ptr+1L),2]<-c(0,-1)

    ConstraintMatrix_name[row_ptr:(row_ptr+1L)]<-c(paste0(term_names[obj_id],"(y)"),
                                                   paste0(term_names[obj_id],"(y+delta)"))
    row_ptr<-row_ptr+2L
  }

  A_binary <- Matrix::sparseMatrix(
    i = rows_i_b, j = rows_j_b, x = rows_x_b,
    dims = c(n_rows, n_bin)
  )
  colnames(A_binary)<-colnames(A_list[[1]])

  # Specification: 1 or 2 rows per term
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
                          C_real   = c(1,-1),
                          operator = operators,
                          d      = rhs,
                          sense = "max",
                          decisionvar_name_new = new_var_name,
                          decisionvar_type_new    = new_var_type,
                          notes  = list(strategy = strategy,
                                        strategy_args = strategy_args,
                                        y_bounds = y_bounds)))
}
