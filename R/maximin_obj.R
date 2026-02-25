#' @title Maximin objective (with optional overflow band)
#'
#' @description
#' Implements a **maximin** objective that maximizes a common floor value
#' \eqn{y} across \eqn{K} linear objective scores \eqn{u_k = a_k^\top x}.
#'
#' The compiled constraints enforce:
#'
#' \deqn{
#'   a_k^\top x \ge p_k\, y, \qquad k = 1,\dots,K,
#' }
#'
#' and, if an overflow bound \code{delta} is finite for term \eqn{k},
#'
#' \deqn{
#'   a_k^\top x \le p_k\, y + \delta_k.
#' }
#'
#' @param x An object of class \code{mstATA_design} created by \code{mst_design()}.
#' @param multiple_terms A list of objective terms created by \code{objective_term()}.
#' The list must contain **two or more** objective terms.
#' All objective terms must be **relative objectives**
#' (i.e., \code{goal = NULL}).
#' @param strategy_args A named list of strategy-specific arguments used by the
#' \strong{maximin} aggregation.
#' Supported fields:
#' \itemize{
#'   \item \code{proportions} — A positive numeric vector of length
#'   \code{length(multiple_terms)} specifying the relative scaling
#'   of each objective term. Defaults to a vector of ones.
#'
#'   \item \code{delta} — A nonnegative numeric value controlling the
#'   overflow band. May be either a scalar or a numeric vector of length
#'   \code{length(multiple_terms)}. Each element must be strictly positive
#'   or \code{Inf}. Defaults to \code{Inf} (no overflow control).
#' }
#'
#' @details
#'
#' **1. Overview**
#'
#' The maximin strategy applies when there are **two or more objective terms**.
#' Each objective term represents a linear score of the form
#'
#' \deqn{
#'   y_k = a_k^\top x,
#' }
#'
#' constructed by \code{objective_term()}.
#'
#' The goal is to maximize the minimum (normalized) score across all terms,
#' ensuring balanced performance rather than optimizing any single criterion.
#'
#' Key characteristics for each objective term:
#'
#' (1) Attribute type
#'
#' Each objective term may be defined using either:
#'
#' \itemize{
#'   \item a **categorical attribute** — maximizing or minimizing the number of
#'         items belonging to a specific category level; or
#'   \item a **quantitative attribute** — maximizing or minimizing the sum of
#'         item-level attribute values (e.g., difficulty, discrimination,
#'         item information function values).
#' }
#'
#' (2) Application level
#'
#' Objective terms may be applied at one of three hierarchical levels:
#'
#' \itemize{
#'   \item \strong{"Module-level"} — only items in a specified module contribute;
#'   \item \strong{"Pathway-level"} — items in all modules belonging to one pathway
#'         contribute; and
#'   \item \strong{"Panel-level"} — all item selections in the panel contribute.
#' }
#'
#' (3) Objective type
#'
#' The maximin strategy supports **relative objectives only**:
#'
#' \itemize{
#'   \item \strong{Relative objective}: maximize \eqn{a_k^\top x}.
#' }
#'
#' Absolute (goal-based) objectives of the form
#' \eqn{|a_k^\top x - g_k|} are not permitted under the maximin strategy.
#'
#' **2.  Proportions and normalization**
#'
#' Each objective term may be associated with a positive proportion \eqn{p_k}.
#' These proportions normalize scores that may be on different scales and encode
#' the relative importance of each objective.
#'
#' By default, \eqn{p_k = 1} for all terms.
#'
#' **3. Overflow control via \code{delta}**
#'
#' Two formulations are supported depending on the value of \code{delta}:
#'
#' \itemize{
#'
#'   \item \strong{Without overflow control} (\code{delta = Inf}):
#'
#'   \deqn{
#'     a_k^\top x \ge p_k \, y, \quad k = 1, \ldots, K.
#'   }
#'
#'   This is the classical maximin formulation, enforcing only a lower bound
#'   on each objective score.
#'
#'   \item \strong{With overflow control} (\code{delta < Inf}):
#'
#'   \deqn{
#'     p_k \, y \le a_k^\top x \le p_k \, y + \delta_k,
#'     \quad k = 1, \ldots, K.
#'   }
#'
#'   This bounded formulation limits how far any single objective score may exceed
#'   the common floor, improving balance and numerical stability.
#' }
#'
#'
#' @section Mathematical Formulation:
#'
#' Let \eqn{x} denote the vector of binary item–module–panel decision variables.
#' For each objective term \eqn{k = 1,\ldots,K}, define:
#'
#' \deqn{
#'   y_k = a_k^\top x.
#' }
#'
#' Introduce a continuous auxiliary variable \eqn{y} representing the minimum
#' normalized score. The maximin optimization problem is:
#'
#' \deqn{
#'   \max \; y
#' }
#'
#' subject to:
#'
#' \deqn{
#'   y_k \ge p_k \, y, \quad k = 1,\ldots,K,
#' }
#'
#' and optionally,
#'
#' \deqn{
#'   y_k \le p_k \, y + \delta_k, \quad k = 1,\ldots,K.
#' }
#'
#'
#' @return A list of class \code{"compiled_objective"}
#' \describe{
#'   \item{name}{A character vector indicating the specifications in each row of `A_binary`}
#'   \item{specification}{A \code{data.frame} including "Requirement","Attribute","Type","Application Level","Operator","Num of Constraints"}
#'   \item{A_binary}{A sparse constraint matrix (class \code{"Matrix"}) for binary
#'   decision variables. Rows correspond to constraints; columns correspond to
#'   binary decision variables.}
#'   \item{A_real}{A sparse constraint matrix (class \code{"Matrix"}) for
#'   continuous decision variables. Must have the same number of rows as
#'   \code{A_binary}.}
#'   \item{operators}{A character vector of constraint operators, one per row of `A_binary`.}
#'   \item{d}{A numeric vector of right-hand-side values for the constraints.}
#'   \item{C_binary}{A numeric vector of objective coefficients for binary decision
#'   variables.}
#'   \item{C_real}{A numeric vector of objective coefficients for continuous
#'   decision variables.}
#'   \item{sense}{Always \code{"max"} for the maximin strategy.}
#'   \item{decisionvar_name_new}{Character vector indicating the names of new decision variables, same length as `ncol(A_real)`.}
#'   \item{decisionvar_type_new}{A character vector of `"C"` (continuous), same length as `ncol(A_real)`.}
#'   \item{notes}{List of metadata.}
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
#' maximin_obj(x = test_mstATA, multiple_terms = list(theta_n1, theta_0, theta_1))
#'
#' @export
#'

maximin_obj <- function(x,multiple_terms,strategy_args = list()) {
  if (!inherits(x, "mstATA_design")) {
    stop("Input 'x' must be an object of class 'mstATA_design'.")
  }
  return(compile_objective(x = x,obj_set = multiple_terms,
                           strategy = "maximin",strategy_args = strategy_args))
}







