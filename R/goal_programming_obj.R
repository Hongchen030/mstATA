#' @title Minimize Deviations from Target Goals (Goal Programming)
#'
#' @description
#' Implements a **goal programming** objective that minimizes deviations between
#' achieved objective values and user-specified target goals.
#'
#' Multiple absolute objective terms are combined into a single optimization
#' problem. Each term contributes one (\code{mode = "one_dev"}) or
#' two (\code{mode = "two_dev"}) deviation variables, and the solver
#' minimizes a (weighted) sum of these deviations.
#'
#' **Weights may be assigned to individual objective terms** to reflect their
#' relative importance.
#'
#' @param x An object of class \code{mstATA_design} created by \code{mst_design()}.
#' @param multiple_terms A list of objective terms created by
#' \code{objective_term()}. The list must contain **two or more**
#' objective terms. All objective terms must be **absolute (goal-based) objectives**,
#' meaning that a finite target value \code{goal} is specified
#' for each term.
#' @param strategy_args A named list of strategy-specific arguments used by the
#' \strong{goal_programming} aggregation.
#'
#' Supported fields:
#' \itemize{
#'   \item \code{mode} — Character string specifying the deviation formulation.
#'   Must be either \code{"one_dev"} (single absolute deviation) or
#'   \code{"two_dev"} (separate positive and negative deviations).
#'
#'   \item \code{weights} — Optional positive numeric vector of length
#'   \code{length(multiple_term)} specifying the relative importance of each
#'   objective term. Defaults to a vector of ones.
#' }
#'
#' @details
#'
#' **1. Overview**
#'
#' Goal programming applies when there are **two or more objective terms**, each
#' formulated as an **absolute deviation objective** of the form
#'
#' \deqn{
#'   |a_k^\top x - g_k|,
#' }
#'
#' where:
#' \itemize{
#'   \item \eqn{a_k^\top x} is the linear score defined by the objective term;
#'   \item \eqn{g_k} is the target value specified by the test developer.
#' }
#'
#' Each objective term is created using \code{objective_term()} and may differ
#' in attribute type, attribute level, and target value.
#'
#' Key characteristics for each objective term:
#'
#' (1) Requirement type
#'
#' \itemize{
#'   \item \strong{Categorical}: minimize deviation from a target number of items
#'         belonging to a specified category level; or
#'   \item \strong{Quantitative}: minimize deviation from a target sum of
#'         item-level quantitative attributes (e.g., difficulty, discrimination,
#'         or item information).
#' }
#'
#' (2) Application level
#'
#' \itemize{
#'   \item \strong{"Module-level"} — only items in a specified module contribute;
#'   \item \strong{"Pathway-level"} — items in all modules belonging to a pathway
#'         contribute; and
#'   \item \strong{"Panel-level"} — all item selections in the panel contribute.
#' }
#'
#' (3) Objective type
#'
#' The goal_programming strategy supports **absolute objectives only**.
#'
#' **2. Deviation Modes**
#'
#' Two deviation formulations are supported:
#'
#' \itemize{
#'   \item \strong{One-deviation mode} (\code{mode = "one_dev"}):
#'
#'   Introduces one nonnegative deviation variable \eqn{d_k \ge 0} for each term:
#'
#'   \deqn{
#'     |a_k^\top x - g_k| \le d_k.
#'   }
#'
#'   The objective minimizes:
#'   \deqn{
#'     \min \sum_{k=1}^K w_k d_k.
#'   }
#'
#'   \item \strong{Two-deviation mode} (\code{mode = "two_dev"}):
#'
#'   Introduces separate positive and negative deviation variables
#'   \eqn{d_k^+ \ge 0} and \eqn{d_k^- \ge 0}:
#'
#'   \deqn{
#'     a_k^\top x - g_k = d_k^+ - d_k^-.
#'   }
#'
#'   with constraints:
#'   \deqn{
#'     a_k^\top x - g_k \le d_k^+, \qquad
#'     g_k - a_k^\top x \le d_k^-.
#'   }
#'
#'   The objective minimizes:
#'   \deqn{
#'     \min \sum_{k=1}^K w_k (d_k^+ + d_k^-).
#'   }
#' }
#'
#' \strong{Interpretation:}
#' \itemize{
#'   \item \eqn{d_k^+} measures how much objective \eqn{k} exceeds its target;
#'   \item \eqn{d_k^-} measures how much objective \eqn{k} falls short;
#'   \item \eqn{d_k} (one-dev) measures total absolute deviation.
#' }
#'
#' @section Mathematical Formulation:
#'
#' For each objective term \eqn{k = 1, \ldots, K}:
#'
#' \deqn{
#'   y_k = a_k^\top x,
#' }
#'
#' where:
#' \itemize{
#'   \item \eqn{a_k} is the coefficient vector defined by the attribute,
#'         level, and application scope;
#'   \item \eqn{x} is the vector of binary item–module–panel decision variables;
#'   \item \eqn{g_k} is the target value.
#' }
#'
#' Auxiliary deviation variables are introduced to linearize
#' \eqn{|a_k^\top x - g_k|}, and the solver minimizes a weighted sum of these
#' deviations.
#'
#' All goal programming problems are compiled as **minimization** problems.
#'
#' @return An object of S3 class \code{"compiled_objective"} with named elements:
#' \describe{
#'   \item{name}{A string indicating the objective function name}
#'   \item{specification}{A \code{data.frame} summarizing the constraint specification, including
#'    the requirement name, attribute, constraint type, application level,
#'    operator, and the number of constraint rows generated.}
#'   \item{A_binary}{A sparse binary matrix representing the linear constraint coefficients.}
#'   \item{A_real}{A sparse constraint matrix (class \code{"Matrix"}) for
#'   continuous decision variables. Must have the same number of rows as
#'   \code{A_binary}.}
#'   \item{C_binary}{A numeric vector of objective coefficients for binary decision
#'   variables.}
#'   \item{C_real}{A numeric vector of objective coefficients for continuous
#'   decision variables.}
#'   \item{operators}{A character vector of constraint operators, one per row of `A_binary`.}
#'   \item{d}{A numeric vector of right-hand-side values for the constraints.}
#'   \item{sense}{Always \code{"min"} for the goal_programming strategy.}
#'   \item{decisionvar_type_new}{A character vector of `"B"` (binary), same length as `ncol(A_real)`.}
#'   \item{decisionvar_name_new}{Character vector indicating the names of decision variables, same length as `ncol(A_real)`.}
#'   \item{notes}{information about "strategy" (`"goal_programming"`) and "strategy_args" (`"mode"`,`"weights"`)}
#' }
#' @examples
#' data("mini_itempool")
#'
#' test_mstATA <- mst_design(
#'   itempool        = mini_itempool,
#'   design          = "1-3-3",
#'   exclude_pathway = c("1-1-3", "1-3-1"),
#'   pathway_length  = 8
#' )
#'
#' obj1 <- objective_term(
#'   test_mstATA, "iif(theta=-1)", NULL,
#'   "Module-level", which_module = 1,
#'   sense = "min", goal = 12
#' )
#'
#' obj2 <- objective_term(
#'   test_mstATA, "iif(theta=0)", NULL,
#'   "Module-level", which_module = 1,
#'   sense = "min", goal = 15
#' )
#'
#' obj3 <- objective_term(
#'   test_mstATA, "iif(theta=1)", NULL,
#'   "Module-level", which_module = 1,
#'   sense = "min", goal = 12
#' )
#'
#' # Example 1: One deviation per goal, equal weights
#' goal_programming_obj(
#'   x = test_mstATA,
#'   multiple_terms = list(obj1, obj2, obj3),
#'   strategy_args = list(mode = "one_dev",weights = NULL))
#'
#' # Example 2: One deviation per goal, unequal weights
#' goal_programming_obj(
#'   x = test_mstATA,
#'   multiple_terms =  list(obj1, obj2, obj3),
#'   strategy_args = list(mode = "one_dev",weights = c(1,0.5,1)))
#'
#' # Example 3: Two deviations per goal
#' goal_programming_obj(
#'   x = test_mstATA,
#'   multiple_terms = list(obj1, obj2, obj3),
#'   strategy_args = list(mode = "two_dev",weights = NULL))
#'
#' @export
#'
goal_programming_obj<-function(x,multiple_terms,strategy_args = list()){
  if (!inherits(x, "mstATA_design")) {
    stop("Input 'x' must be an object of class 'mstATA_design'.")
  }
  return(compile_objective(x = x,obj_set = multiple_terms,
                           strategy = "goal_programming",strategy_args = strategy_args))
}
