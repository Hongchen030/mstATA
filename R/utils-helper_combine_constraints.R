#' @title  Combine multiple `mstATA_constraint` Objects
#'
#' @description
#'
#' Combines a list of constraints into a single unified `mstATA_constraint` object.
#' All input constraints must be of class `mstATA_constraint`
#'
#' @param constraints A list of constraint objects of class `"mstATA_constraint"`
#'
#' @return A single constraint object of class `"mstATA_constraint"` created using \code{create_constraint()},
#'   with stacked matrices and concatenated metadata.
#' @keywords internal
#' @noRd


combine_constraints <- function(constraints) {

  if (!is.list(constraints) || length(constraints) == 0) {
    stop("'constraints' must be a non-empty list.", call. = FALSE)
  }

  if (!all(vapply(constraints, inherits, logical(1L), "mstATA_constraint"))) {
    stop("All elements of 'constraints' must be of class 'mstATA_constraint'.",
         call. = FALSE)
  }

  all_bin_vars <- unique(unlist(lapply(constraints, function(c) {
    if (is.null(c$A_binary) || is.null(colnames(c$A_binary))) {
      stop("All constraints must define 'A_binary' with column names.",
           call. = FALSE)
    }
    colnames(c$A_binary)
  })))

  if (!all(vapply(constraints, function(c)
    setequal(colnames(c$A_binary), all_bin_vars), logical(1L)))) {
    message("Constraints use different subsets of binary decision variables; alignment applied.")
  }


  all_real_vars <- unique(unlist(lapply(constraints,function(c) {
      if (is.null(c$A_real) || ncol(c$A_real) == 0) character(0)
      else colnames(c$A_real)
    }
  )))

  if (length(all_real_vars) > 0) {
    stop(
      "Constraints must not define real-valued decision variables. ",
      "Continuous variables are introduced only through objectives.",
      call. = FALSE
    )
  }

  A_binary_all <- do.call(rbind, lapply(constraints, function(c)
    align_binary_to_universe(c$A_binary, all_bin_vars)
  ))

  operators     <- unlist(lapply(constraints, `[[`, "operators"))
  d             <- unlist(lapply(constraints, `[[`, "d"))
  name          <- unlist(lapply(constraints, `[[`, "name"))
  specification <- do.call(rbind, lapply(constraints, `[[`, "specification"))

  if (length(operators) != nrow(A_binary_all)) {
    stop("Mismatch between constraint rows and operators.", call. = FALSE)
  }

  if(length(d) !=nrow(A_binary_all)){
    stop("Mismatch between constraint rows and d.", call. = FALSE)
  }

  if(length(name) !=nrow(A_binary_all)){
    stop("Mismatch between constraint rows and name.", call. = FALSE)
  }

  return(create_constraint(name = name,specification = specification,
                           A_binary = A_binary_all,A_real = NULL,
                           operators = operators,d = d,
                           C_binary = NULL,
                           C_real = NULL))
}

