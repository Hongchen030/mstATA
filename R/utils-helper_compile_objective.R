#' @title Compile objective_set into linear pieces for the MILP
#' @description
#' Dispatches to strategy-specific compilers.
#' @return An object of S3 class \code{"compiled objective"} with named elements:
#' \describe{
#'   \item{name}{A character vector indicating the specifications in each row of `A_binary`}
#'   \item{specification}{A \code{data.frame} summarizing the constraint specification, including
#'    the requirement name, attribute, constraint type, application level,
#'    operator, and the number of constraint rows generated.}
#'   \item{A_binary}{A sparse binary matrix representing the linear constraint coefficients.}
#'   \item{A_real}{Sparse matrix of coefficients for the real decision variables (if any)}
#'   \item{C_binary}{Penalty vector for binary variables (if any)}
#'   \item{C_real}{Penalty vector for real variables (if any)}
#'   \item{operators}{A character vector of constraint operators, one per row of `A_binary`.}
#'   \item{d}{A numeric vector of right-hand-side values for the constraints.}
#'   \item{sense}{Either "min" or "max"}
#'   \item{decisionvar_name_new}{Character vector indicating the names of new decision variables, same length as `ncol(A_real)`.}
#'   \item{decisionvar_type_new}{A character vector of `"C"` (continuous), same length as `ncol(A_real)`.}
#'   \item{notes}{A list containing the strategy name, strategy arguments, and \code{y_bounds}.}
#' }
#' @noRd
#' @keywords internal
compile_objective <- function(x, obj_set,strategy,strategy_args = list()) {
  s<-strategy
  switch(s,
         "single"  = compile_single_(x, obj_set),
         "weighted_sum" = compile_weighted_sum_(x, obj_set,strategy,strategy_args),
         "capped_maximin" = compile_capped_maximin_(x,obj_set,strategy,strategy_args),
         "maximin" = compile_maximin_(x, obj_set,strategy,strategy_args),
         "goal_programming" = compile_goal_programming_(x, obj_set,strategy,strategy_args),
         "minimax" = compile_minimax_(x, obj_set,strategy,strategy_args))
}



