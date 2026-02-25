#' @title Create an mstATA Optimization Model Object
#'
#' @description
#' Constructs an \code{"mstATA_model"} object that encapsulates a mixed-integer
#' linear optimization model used in automated test assembly (ATA).
#'
#' The model consists of linear constraints defined over binary and continuous
#' decision variables, optional model specifications for bookkeeping, and
#' optimization metadata (objective sense and variable bounds). This function
#' performs extensive input validation to ensure internal consistency between
#' constraint matrices, decision variables, and bounds.
#'
#' @param name A character vector naming each constraint. Length must equal the
#'   number of constraints.
#' @param specification An optional data frame describing the model structure
#'   (e.g., constraint blocks). If provided and containing a column named
#'   `"Num of Constraints"`, its sum must equal the total number of constraints.
#' @param A_binary A sparse constraint matrix (class \code{"Matrix"}) for binary
#'   decision variables. Rows correspond to constraints; columns correspond to
#'   binary decision variables.
#' @param A_real An optional sparse constraint matrix (class \code{"Matrix"}) for
#'   continuous decision variables. Must have the same number of rows as
#'   \code{A_binary}.
#' @param C_binary A numeric vector of objective coefficients for binary decision
#'   variables.
#' @param C_real A numeric vector of objective coefficients for continuous
#'   decision variables.
#' @param operators A character vector of relational operators for constraints
#'   (e.g., \code{"<="}, \code{">="}, \code{"=="}).
#' @param d A numeric vector specifying the right-hand side of each constraint.
#' @param decisionvar_name A character vector of decision variable names.
#' @param decisionvar_type A character vector indicating the type of each
#'   decision variable: \code{"B"} for binary or \code{"C"} for continuous.
#' @param sense Optimization sense: either \code{"min"} (minimization) or
#'   \code{"max"} (maximization).
#' @param lb_bound A numeric vector of lower bounds for continuous decision
#'   variables.
#' @param ub_bound A numeric vector of upper bounds for continuous decision
#'   variables.
#'
#' @return
#' An object of class \code{"mstATA_model"}, represented as a named list
#' containing constraint matrices, objective coefficients, decision variable
#' metadata, bounds, and optimization sense.
#' \describe{
#'   \item{name}{A character vector indicating the specifications in each row of `A_binary`}
#'   \item{specification}{A \code{data.frame} summarizing the constraint specification, including
#'    the requirement name, attribute, constraint type, application level,
#'    operator, and the number of constraint rows generated.}
#'   \item{A_binary}{A sparse constraint matrix (class \code{"Matrix"}) for binary
#'   decision variables. Rows correspond to constraints; columns correspond to
#'   binary decision variables.}
#'   \item{A_real}{A sparse constraint matrix (class \code{"Matrix"}) for
#'   continuous decision variables. Must have the same number of rows as
#'   \code{A_binary}.}
#'   \item{A}{Constraint coefficient matrix (sparse or dense).}
#'   \item{C_binary}{A numeric vector of objective coefficients for binary decision
#'   variables.}
#'   \item{C_real}{A numeric vector of objective coefficients for continuous
#'   decision variables.}
#'   \item{obj}{Objective function coefficient vector.}
#'   \item{d}{Right-hand-side vector of constraints.}
#'   \item{operators}{Constraint operators (e.g., \code{"<="}, \code{">="}, \code{"="}).}
#'   \item{vtype}{Decision variable types (\code{"B"} for binary, \code{"C"} for continuous).}
#'   \item{varname}{Decision variable names.}
#'   \item{lb}{Lower bounds for decision variables.}
#'   \item{ub}{Upper bounds for decision variables.}
#'   \item{sense}{Optimization sense (\code{"min"} or \code{"max"}).}
#' }
#' @details
#' The constraint system is represented as:
#'
#' \deqn{
#'   A_B x_B + A_C x_C \; (\text{operator}) \; b
#' }
#'
#' where \eqn{x_B} are binary decision variables and \eqn{x_C} are continuous
#' decision variables.
#'
#' The objective function is defined as:
#'
#' \deqn{
#'   \min/\max \; C_B^\top x_B + C_C^\top x_C
#' }
#'
#' Continuous decision variables are subject to lower and upper bounds specified
#' by \code{lb_bound} and \code{ub_bound}. Binary variables are implicitly bounded
#' in \eqn{\{0, 1\}}.
#'
#' @keywords internal

create_model <- function(name,specification,
                         A_binary,A_real,
                         C_binary,C_real,
                         operators,d,
                         decisionvar_name,decisionvar_type,
                         sense,
                         lb_bound,ub_bound) {

  if (!inherits(A_binary, "Matrix")){
    stop("A_binary must be a sparse Matrix.")
  }
  if(!is.null(A_real)){
    if (!inherits(A_real, "Matrix")) {
      stop("'A_real' must be a sparse Matrix.")
    }
  }

  operators<-check_operator(operator = operators)
  if (!is.numeric(d)) {
    stop("'d' must be numeric.")
  }
  if (!is.character(decisionvar_name)) {
    stop("'decisionvar_name' must be a character vector.")
  }
  if (!is.character(decisionvar_type) || !all(decisionvar_type %in% c("B", "C"))) {
    stop("'decisionvar_type' must contain only 'B' and 'C'.")
  }
  if (!sense %in% c("min", "max")) {
    stop("'sense' must be either 'min' or 'max'.")
  }

  n_cons <- nrow(A_binary)
  if (length(d) != n_cons || length(operators) != n_cons || length(name) !=n_cons) {
    stop("Length of 'd', 'name' and 'operators' must match the number of rows in A_binary.")
  }
  if(!is.null(A_real) && nrow(A_real) != n_cons){
    stop("nrow(A_real) must match the number of rows in A_binary.")
  }

  idx_B <- which(decisionvar_type == "B")
  idx_C <- which(decisionvar_type == "C")
  if (ncol(A_binary) != length(idx_B)) {
    stop("ncol(A_binary) must equal number of binary decision variables.")
  }
  if (!is.null(A_real) && ncol(A_real) != length(idx_C)) {
    stop("ncol(A_real) must equal number of continuous decision variables.")
  }
  if (length(C_binary) != length(idx_B)) {
    stop("length(C_binary) must equal number of binary decision variables.")
  }
  if (length(C_real) != length(idx_C)) {
    stop("length(C_real) must equal number of continuous decision variables.")
  }

  if(is.null(lb_bound)){
    lb_bound<- rep(-Inf,length(idx_C))
  }
  if(is.null(ub_bound)){
    ub_bound<-rep(Inf,length(idx_C))
  }

  if (length(lb_bound) != length(idx_C) || length(ub_bound) != length(idx_C)) {
    stop("lb_bound and ub_bound must match number of continuous variables.")
  }

  if (is.data.frame(specification) && "Num of Constraints"%in%names(specification)) {
    if (sum(specification$`Num of Constraints`) != n_cons) {
      stop("Sum of 'Num of Constraints' in specification must equal ncol(A_binary).")
    }
  }

  A <- if (is.null(A_real)) {
    A_binary
  } else {
    cbind(A_binary, A_real)
  }


  obj <- if (is.null(C_real)) {
    C_binary
  } else {
    c(C_binary,C_real)
  }

  lb<-ub<-numeric(length(decisionvar_type))
  lb[idx_B]<-0
  ub[idx_B]<-1
  lb[idx_C]<-lb_bound
  ub[idx_C]<-ub_bound

  model <- list(name = name,specification = specification,
                A_binary = A_binary,A_real = A_real,A = A,
                C_binary = C_binary, C_real  = C_real, obj = obj,
                operators = operators,d = d,
                varname  = decisionvar_name,
                vtype  = decisionvar_type,
                lb          = lb,
                ub          = ub,
                sense   = sense)

  class(model) <- "mstATA_model"
  return(model)
}
