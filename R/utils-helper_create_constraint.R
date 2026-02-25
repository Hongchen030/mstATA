#' @title Construct a Compiled Constraint
#'
#' @description
#'
#' This function creates a generic S3 \code{"mstATA_constraint"} object used in the constraint-builder framework
#' for automated test assembly.
#'
#' @param name A character vector indicating the specifications in each row of `A_binary` (default is NULL).
#' @param specification A \code{data.frame} including "Requirement","Attribute","Type","Application Level","Operator","Num of Constraints".
#'  Default is `NULL`.
#' @param A_binary A sparse binary matrix (class \code{"dgCMatrix"}) representing constraint coefficients
#'   for binary decision variables. Each row corresponds to one constraint.
#' @param A_real A sparse real-valued matrix (optional) representing constraint coefficients
#'   for continuous decision variables.
#' @param operators A character vector of constraint operators (e.g., \code{"="}, \code{"<="}, \code{">="}).
#' @param d A numeric vector of right-hand side (RHS) values for the constraints.
#' @param C_binary Optional penalty vector for binary decision variables (default is NULL).
#' @param C_real Optional penalty vector for real decision variables (default is NULL).
#'
#' @return An object of S3 class \code{"mstATA_constraint"} with the following named elements:
#' \describe{
#'   \item{name}{A character vector indicating the specifications in each row of `A_binary`}
#'   \item{specification}{A \code{data.frame} including "Requirement","Attribute","Type","Application Level","Operator","Num of Constraints"}
#'   \item{A_binary}{Sparse matrix of binary coefficients}
#'   \item{A_real}{NULL for 'mstATA_constraint' object}
#'   \item{operators}{Vector of relational operators}
#'   \item{d}{Right-hand side vector}
#'   \item{C_binary}{NULL for 'mstATA_constraint' object}
#'   \item{C_real}{NULL for 'mstATA_constraint' object}
#'   \item{sense}{NULL for 'mstATA_constraint' object}
#' }
#'
#' @keywords internal
#' @noRd


create_constraint <- function(name = NULL, specification = NULL,
                              A_binary,A_real=NULL,operators,d,
                              C_binary=NULL,C_real=NULL) {
  if (!inherits(A_binary, "Matrix")){
    stop("A_binary must be a sparse Matrix.")
  }

  n_cons <- nrow(A_binary)
  operators<-check_operator(operator = operators)
  if (!is.numeric(d)) {
    stop("'d' must be numeric.")
  }
  if (length(d) != n_cons || length(operators) != n_cons) {
    stop("Length of 'd' and 'operators' must match the number of rows in A_binary.")
  }
  if (!is.null(A_real)) {
    stop("A_real must be NULL for mstATA_constraints. ",
         "Continuous decision variables are introduced only by objectives, ",
         "not by constraints.",
      call. = FALSE)
  }

  if(!is.null(specification) && !is.null(name)){
    name_len<-length(name)
    num_con<-sum(specification$`Num of Constraints`)
    if(name_len!=num_con || name_len!=n_cons){
      stop("Number of constraints must match the number of rows in A_binary.")
    }
  }

  structure(list(name = name, specification = specification,
                 A_binary = A_binary,A_real=NULL,
                 operators = operators,d = d,
                 C_binary = NULL,C_real=NULL,
                 sense = NULL),
            class="mstATA_constraint")
}

