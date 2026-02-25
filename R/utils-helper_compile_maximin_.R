#' @title Compile a Maximin Objective
#'
#' @description
#' Implements the **maximin** objective aggregation strategy, which maximizes the
#' minimum normalized score across multiple objective terms.
#'
#' The optimization problem is:
#'
#' \deqn{
#'   \max \; \min_{k = 1,\ldots,K} \left( \frac{a_k^\top x}{p_k} \right),
#' }
#'
#' where each objective term is a linear score \eqn{a_k^\top x}, and
#' \eqn{p_k > 0} is a user-specified (or default) proportions that scales the
#' relative importance of term \eqn{k}.
#'
#' This function is automatically called by \code{compile_objective()} when
#' \code{strategy = "maximin"}.
#'
#' @param x
#' An object of class \code{mstATA_design} created by \code{mst_design()}.
#' @param obj_set A list containing multiple \code{objective_term()}
#'   objects. The function will throw an error if the list does not have
#'   length greater than 1. Each term must be a *relative* objective (i.e., no goal values).
#'
#' @param strategy A character string specifying the optimization strategy.
#' Currently, only \code{"maximin"} is supported.
#'
#' @param strategy_args
#' A list of strategy-specific arguments. For the \code{"maximin"} strategy,
#' \code{strategy_args} must be either an empty list or
#' \code{list(proportions = proportions, delta = delta)}.
#'
#' The argument \code{proportions} must be a positive numeric vector of length
#' \code{length(obj_set)}. If not provided, it defaults to
#' \code{rep(1, length(obj_set))}.
#'
#' The argument \code{delta} controls overflow and may be a positive numeric
#' value or \code{Inf}. It must have length 1 or \code{length(obj_set)}.
#' If not provided, \code{delta} defaults to \code{Inf}, in which case no
#' overflow control is applied.
#'
#'
#' @details
#'
#' **Optimization Formulation**
#'
#' Let there be \eqn{K} objective terms, each defined as a linear score:
#'
#' \deqn{
#'   y_k = a_k^\top x, \quad k = 1, \ldots, K,
#' }
#'
#' where:
#'
#' \itemize{
#'   \item \eqn{a_k} is the coefficient vector constructed by
#'         \code{objective_term()};
#'   \item \eqn{x} denotes the binary item–module–panel decision variables;
#'   \item \eqn{p_k > 0} is the proportions used to normalize objective \eqn{k}.
#' }
#'
#' The maximin strategy introduces a common auxiliary variable \eqn{y} representing
#' the minimum normalized score across all objectives, and maximizes this value.
#'
#' \deqn{
#'   \max \; y
#' }
#'
#' subject to:
#'
#' \deqn{
#'   \frac{a_k^\top x}{p_k} \ge y, \quad k = 1, \ldots, K.
#' }
#'
#' ## Overflow Control via \code{delta}
#'
#' Two formulations are supported depending on the value of \code{delta}.
#'
#' \itemize{
#'
#'   \item \strong{Without overflow control} (\code{delta = Inf}):
#'
#'   \deqn{
#'     a_k^\top x \ge p_k \, y, \quad k = 1, \ldots, K.
#'   }
#'
#'   This is the classical maximin formulation, which only enforces a lower bound
#'   on each objective term.
#'
#'   \item \strong{With overflow control} (\code{delta < Inf}):
#'
#'   \deqn{
#'     p_k \, y \le a_k^\top x \le p_k \, y + \delta_k,
#'     \quad k = 1, \ldots, K.
#'   }
#'
#'   This bounded formulation prevents any single objective from excessively
#'   exceeding the common minimum by more than \eqn{\delta_k}, improving numerical
#'   stability and solution balance.
#'
#'   It is worth noting that when \code{delta} is included, the key difference
#'   between the \code{"capped_maximin"} and \code{"maximin"} strategies lies in
#'   how \code{delta} is treated in the optimization formulation.
#'
#'   For \code{"capped_maximin"}, \code{delta} appears directly in the objective function.
#'   In contrast, for
#'   \code{"maximin"}, \code{delta} does not appear in the objective function.
#' }
#'
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
#'   \item{sense}{Always \code{"max"} for the maximin strategy.}
#'   \item{decisionvar_name_new}{Character vector indicating the names of new decision variables, same length as `ncol(A_real)`.}
#'   \item{decisionvar_type_new}{A character vector of `"C"` (continuous), same length as `ncol(A_real)`.}
#'   \item{notes}{A list containing the strategy name, strategy arguments, and \code{y_bounds}.}
#' }
#' @keywords internal
#'

compile_maximin_ <- function(x, obj_set, strategy = "maximin",
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

  strategy <- match.arg(strategy,"maximin")
  num_objs <- length(obj_set)
  if (num_objs == 1L) {
    stop("Multiple-term strategies require at least two terms. Use 'single' instead.")
  }

  term_names <- vapply(obj_set, function(t) t$name,character(1L))
  term_goals <- unlist(lapply(obj_set, `[[`, "goal"))
  if (anyDuplicated(term_names)) stop("All objective term names must be unique.")

  terms_senses <- vapply(obj_set, function(t) t$sense, character(1L))
  if (any(terms_senses == "min")) {
    stop(
      "maximin requires all objective terms to have sense = 'max'.",
      call. = FALSE
    )
  }

  strategy_args <- validate_strategy_args(strategy, strategy_args, n_terms = num_objs,
                                          goals = term_goals)
  delta <- strategy_args$delta
  proportions <- strategy_args$proportions

  decisionvar_name<-x$decisionvar_name
  n_bin   <- length(decisionvar_name)
  use_cap <- any(is.finite(delta))

  A_list  <- lapply(obj_set, `[[`, "coef_val")
  new_var_name<-"y_maximin"
  new_var_type<-"C"

  term_bounds <- lapply(A_list, function(a) estimate_term_bounds(x = x, a_vec = a))
  lb   <- vapply(term_bounds, `[[`, numeric(1), "lower_bound")
  ub   <- vapply(term_bounds, `[[`, numeric(1), "upper_bound")
  if(use_cap){
    lo <- max((lb-delta)/proportions)
  }else{
    lo <- max(lb / proportions)
  }
  hi <- min(ub / proportions)
  if (!is.finite(lo)) lo <- -Inf
  if (!is.finite(hi)) hi <-  Inf
  if (is.finite(lo) && is.finite(hi) && lo > hi) {
    # infeasible overlap; keep wide bounds rather than invalid interval
    lo <- -Inf
    hi <-  Inf
  }
  y_bounds <- matrix(c(lo, hi), nrow = 1)

  rows_per_term <- if (use_cap) 2L else 1L
  n_rows <- num_objs * rows_per_term

  nnz_each <- vapply(A_list, function(a) sum(a != 0), integer(1))
  nnz_bin  <- sum(nnz_each) * rows_per_term

  rows_i_b <- integer(nnz_bin)
  rows_j_b <- integer(nnz_bin)
  rows_x_b <- numeric(nnz_bin)

  # A_real is just (-proportions) repeated per row block
  A_real <- Matrix::Matrix(0, nrow = n_rows, ncol = 1, sparse = TRUE)
  colnames(A_real)<-new_var_name

  operators <- character(n_rows)
  rhs       <- numeric(n_rows)
  ConstraintMatrix_name <- character(n_rows)

  row_ptr <- 1L
  bin_ptr <- 1L
  for (obj_id in seq_len(num_objs)) {
    a_vec <- as.vector(A_list[[obj_id]])
    nz_b  <- which(a_vec!=0)
    p_k<-proportions[obj_id]
    # Row 1: a'x - proportions*y >= 0
    idx <- bin_ptr:(bin_ptr + length(nz_b) - 1L)
    rows_i_b[idx] <- row_ptr
    rows_j_b[idx] <- nz_b
    rows_x_b[idx] <- a_vec[nz_b]
    bin_ptr <- bin_ptr + length(nz_b)
    operators[row_ptr] <- ">="
    rhs[row_ptr] <- 0
    ConstraintMatrix_name[row_ptr] <- term_names[obj_id]
    A_real[row_ptr,1]<- -p_k
    if (use_cap) {
      idx <- bin_ptr:(bin_ptr + length(nz_b) - 1L)
      rows_i_b[idx] <- row_ptr+1L
      rows_j_b[idx] <- nz_b
      rows_x_b[idx] <- a_vec[nz_b]
      bin_ptr <- bin_ptr + length(nz_b)
      operators[row_ptr+1L] <- "<="
      rhs[row_ptr+1L] <- delta[obj_id]
      ConstraintMatrix_name[row_ptr+1L] <- paste0(term_names[obj_id], " (cap)")
      A_real[(row_ptr+1L),1]<- -p_k
      row_ptr <- row_ptr + 2L
    } else {
      row_ptr <- row_ptr + 1L
    }
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
        `Num of Constraints` = if (use_cap) 2L else 1L,
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
                          C_real   = 1,
                          operator = operators,
                          d      = rhs,
                          sense = "max",
                          decisionvar_name_new = new_var_name,
                          decisionvar_type_new    = new_var_type,
                          notes  = list(strategy = strategy,
                                        strategy_args = strategy_args,
                                        y_bounds = y_bounds)))
}
