#' @title Capped maximin objective
#'
#' @description
#'
#' The **capped maximin** strategy balances multiple competing objectives by
#' maximizing a common lower bound across multiple objective terms, while simultaneously
#' limiting how far any single objective term may exceed that bound.
#'
#' Compared to the classical maximin formulation—where overflow control may be
#' imposed through a user-specified upper bound \eqn{\delta}—the capped maximin
#' strategy treats \eqn{\delta} as a decision variable and optimizes it
#' jointly with the common floor value \eqn{y}. The resulting objective to be maximized,
#' \eqn{(y - \delta)}, balances improving the worst-performing objective
#' while penalizing excessive dominance by any single objective.
#'
#'
#' @param x An object of class \code{mstATA_design} created by \code{mst_design()}.
#' @param multiple_terms A list of objective terms created by \code{objective_term()}.
#' The list must contain **two or more** objective terms.
#' All objective terms must be **relative objectives**
#' (i.e., \code{goal = NULL}).
#' @param strategy_args A named list of strategy-specific arguments used by the
#' \strong{capped_maximin} aggregation.
#'
#' Supported fields:
#' \itemize{
#'   \item \code{proportions} — A positive numeric vector of length
#'   \code{length(multiple_terms)} specifying the relative importance
#'   of each objective term. Defaults to a vector of ones.
#' }
#'
#' @details
#'
#' **1. Overview**
#'
#' The capped maximin strategy applies when there are **two or more objective
#' terms**, each representing a linear score of the form
#'
#' \deqn{
#'   y_k = a_k^\top x, \qquad k = 1,\ldots,K,
#' }
#'
#' where \eqn{a_k} is the coefficient vector constructed by
#' \code{objective_term()}, and \eqn{x} denotes the binary
#' item–module decision variables.
#'
#' The goal is to:
#'
#' \itemize{
#'   \item maximize the *minimum normalized performance* across all objective
#'         terms, and
#'   \item restrict excessive dominance of any single objective via an overflow
#'         band.
#' }
#'
#' Unlike the classical maximin strategy (where \eqn{\delta} is user-specified or
#' infinite), capped maximin treats \eqn{\delta} as a **decision variable** and
#' optimizes it jointly with the common floor.
#'
#' Key characteristics for each objective term:
#'
#' (1) Requirement type
#'
#' \itemize{
#'   \item \strong{Categorical}: maximize the number of items from a specified
#'         category level; or
#'   \item \strong{Quantitative}: maximize the sum of item-level quantitative
#'         attributes (e.g., difficulty, discrimination, item information).
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
#' The capped maximin strategy supports **relative objectives only**: maximize \eqn{a_k^\top x}
#'
#' Absolute (goal-based) objectives of the form
#' \eqn{|a_k^\top x - g_k|} are **not permitted** under capped maximin.
#'
#'**2. Proportions and normalization**
#'
#' Each objective term may be associated with a positive proportion
#' \eqn{p_k > 0}. These proportions encode the relative importance of each objective.
#'
#' By default, \eqn{p_k = 1} for all terms.
#'
#' **3. Overflow control via \eqn{\delta}**
#'
#' The capped maximin formulation introduces a common overflow variable
#' \eqn{\delta \ge 0}, yielding the constraints:
#'
#' \deqn{
#'   p_k\, y \;\le\; a_k^\top x \;\le\; p_k\, y + \delta,
#'   \qquad k = 1,\ldots,K.
#' }
#'
#' These constraints ensure that:
#'
#' \itemize{
#'   \item all objective terms achieve a highest value proportional to a common normalized floor
#'         \eqn{y}, and
#'   \item no term exceeds the highest value by more than \eqn{\delta}.
#' }
#'
#'
#' @section Mathematical Formulation:
#'
#' Let \eqn{x} denote the vector of binary item–module decision variables.
#' For each objective term \eqn{k = 1,\ldots,K}, define:
#'
#' \deqn{
#'   y_k = a_k^\top x.
#' }
#'
#' Introduce two continuous decision variables:
#'
#' \itemize{
#'   \item \eqn{y} — the common normalized floor value;
#'   \item \eqn{\delta} — the maximum allowable overflow above the floor.
#' }
#'
#' The capped maximin optimization problem is:
#'
#' \deqn{
#'   \max \; (y - \delta)
#' }
#'
#' subject to:
#'
#' \deqn{
#'   p_k \, y \;\le\; y_k \;\le\; p_k \, y + \delta,
#'   \qquad k = 1,\ldots,K.
#' }
#'
#' This formulation encourages balanced improvement across multiple objectives while
#' penalizing excessive disparity among them.
#'
#' @return A list of class \code{"compiled_objective"}
#' \describe{
#'   \item{name}{A character vector indicating the specifications in each row of `A_binary`}
#'   \item{specification}{A \code{data.frame} summarizing the constraint specification, including
#'    the requirement name, attribute, constraint type, application level,
#'    operator, and the number of constraint rows generated.}
#'   \item{A_binary}{A sparse binary matrix representing the linear constraint coefficients.}
#'   \item{A_real}{A sparse constraint matrix (class \code{"Matrix"}) for
#'   continuous decision variables. Must have the same number of rows as
#'   \code{A_binary}.}
#'   \item{operators}{A character vector of constraint operators, one per row of `A_binary`.}
#'   \item{d}{A numeric vector of right-hand-side values for the constraints.}
#'   \item{C_binary}{A numeric vector of objective coefficients for binary decision
#'   variables.}
#'   \item{C_real}{A numeric vector of objective coefficients for continuous
#'   decision variables.}
#'   \item{sense}{Always \code{"max"} for the capped_maximin strategy.}
#'   \item{decisionvar_name_new}{Character vector indicating the names of new decision variables, same length as `ncol(A_real)`.}
#'   \item{decisionvar_type_new}{A character vector of `"C"` (continuous), same length as `ncol(A_real)`.}
#'   \item{notes}{List of strategy metadata.}
#' }
#'
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
#' # Example 1: Maximize the minimum TIF across theta = -1, 0, 1
#' theta_n1 <- objective_term(
#'   x = test_mstATA,
#'   attribute = "iif(theta=-1)",
#'   applied_level = "Module-level",
#'   which_module = 1,
#'   sense = "max"
#' )
#'
#' theta_0 <- objective_term(
#'   x = test_mstATA,
#'   attribute = "iif(theta=0)",
#'   applied_level = "Module-level",
#'   which_module = 1,
#'   sense = "max"
#' )
#'
#' theta_1 <- objective_term(
#'   x = test_mstATA,
#'   attribute = "iif(theta=1)",
#'   applied_level = "Module-level",
#'   which_module = 1,
#'   sense = "max"
#' )
#'
#'
#' capped_maximin_obj(x = test_mstATA, multiple_terms = list(theta_n1,theta_0,theta_1))
#'
#' @export

capped_maximin_obj <- function(x,multiple_terms,strategy_args = list()) {
  if (!inherits(x, "mstATA_design")) {
    stop("Input 'x' must be an object of class 'mstATA_design'.")
  }
  return(compile_objective(x = x,obj_set = multiple_terms,strategy = "capped_maximin",strategy_args = strategy_args))
}
