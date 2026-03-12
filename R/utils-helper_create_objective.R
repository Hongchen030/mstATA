#' @title Construct a Compiled Objective
#'
#' @description
#' This internal helper function assembles a fully normalized objective payload,
#' using a schema parallel to that of compiled constraint objects.
#' It bundles all coefficient matrices, penalty vectors, operators, and metadata
#' into a single structured object that can be consumed by the model compiler.
#'
#' The resulting object has class `"compiled objective"` and is used
#' downstream when constructing the MIP model for MST panel assembly.
#'
#' @param name A character vector indicating the specifications in each row of `A_binary` (default is NULL).
#' @param specification A \code{data.frame} including "Requirement","Attribute","Type","Application Level","Operator","Num of Constraints".
#'  Default is `NULL`.
#' @param A_binary A sparse matrix (or `NULL`) containing coefficients for binary
#'   decision variables. Must have one row per objective constraint.
#' @param A_real A sparse matrix (or `NULL`) containing coefficients for real
#'   decision variables. Must have the same number of rows as `A_binary`
#'   (or zero if both are `NULL`).
#' @param C_binary Optional numeric penalty vector for binary decision variables.
#'   If `NULL`, a zero vector of length `ncol(A_binary)` is used.
#' @param C_real Optional numeric penalty vector for real decision variables.
#'   If `NULL`, a zero vector of length `ncol(A_real)` is used.
#' @param operator Character vector of operators ("<=",">=").
#' @param d      Numeric vector of right-hand sides.
#' @param sense "max" or "min"
#' @param decisionvar_name_new character vector of NEW variable names
#' @param decisionvar_type_new  same length as \code{decisionvar_name_new}; usually "C"
#' @param notes List of metadata.
#' @return list with class "compiled_objective"
#' \describe{
#'   \item{name}{A character vector indicating the specifications in each row of `A_binary`}
#'   \item{specification}{A \code{data.frame} summarizing the constraint specification, including
#'    the requirement name, attribute, constraint type, application level,
#'    operator, and the number of constraint rows generated.}\
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
#' @keywords internal
#' @noRd

create_objective <- function(name = NULL, specification = NULL,
                                    A_binary, A_real,
                                    C_binary=NULL, C_real,
                                    operator = character(0), d = numeric(0),
                                    sense,
                                    decisionvar_name_new = character(0),
                                    decisionvar_type_new    = character(0),
                                    notes = list()){
  sense <- match.arg(sense,c("max","min"))
  operator<-check_operator(operator = operator)

  if (!inherits(A_binary, "Matrix")){
    stop("A_binary must be a sparse Matrix.")
  }
  if (is.null(A_real)) {
    stop("A_real must be non-NULL for a compiled mstATA_objective. ",
         "Continuous decision variables are introduced by objectives.",
         call. = FALSE)
  }

  if (!inherits(A_real, "Matrix")){
    stop("A_real must be a sparse Matrix.")
  }

  n_bin  <- ncol(A_binary)
  n_real <- ncol(A_real)
  nrow_bin  <- nrow(A_binary)
  nrow_real <- nrow(A_real)

  if (is.null(C_binary)) C_binary <- numeric(n_bin)
  stopifnot(length(C_binary) == n_bin)
  if (is.null(C_real))   C_real   <- numeric(n_real)
  stopifnot(length(C_real) == n_real)
  stopifnot(length(decisionvar_name_new) == length(decisionvar_type_new))
  stopifnot(length(operator) == length(d))

  # Each block should have the same number of rows as operator (or zero)
  if (nrow_real!= length(operator)||nrow_bin!=nrow_real || length(operator)!=length(d)) {
    stop(sprintf("Mismatch: A_binary has %d rows, A_real has %d rows, operator has %d, d has %d.",
                 nrow_bin,nrow_real,length(operator),length(d)),
         call. = FALSE)
  }
  if(length(decisionvar_name_new)!=n_real){
    stop(sprintf("A_real has %d columns, continuous decision variables has %d.", ncol(A_real), length(decisionvar_name_new)))
  }

  if(!is.null(specification) && !is.null(name)){
    name_len<-length(name)
    num_con<-sum(specification$`Num of Constraints`)
    if(name_len!=num_con || name_len!=nrow_bin){
      stop("Number of constraints must match the number of rows in A_binary.")
    }
  }

  structure(list(name = name,specification = specification,
                 A_binary = A_binary,A_real = A_real,
                 operators = operator,d = d,
                 C_binary = C_binary, C_real   = C_real,
                 sense = sense,
                 decisionvar_name_new = decisionvar_name_new,
                 decisionvar_type_new = decisionvar_type_new,
                 notes = notes),
            class = "compiled_objective")
}

