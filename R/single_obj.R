#' @title Maximize or Minimize a Single Objective Term
#'
#' @description
#' This function compiles a **single-objective optimization problem** for MST
#' assembly. The optimization is to maximize or minimize a single linear score.
#'
#' \deqn{
#'   \max \; y \quad \text{or} \quad \min \; y,
#' }
#'
#' where \eqn{y = a^\top x} is the linear objective term created by
#' \code{objective_term()}.
#'
#' @param x An object of class \code{mstATA_design} created by \code{mst_design()}.
#' @param single_term A single objective term created by \code{objective_term()}.
#' This function only accepts **one** objective term; supplying multiple
#' terms will result in an error.
#'
#' @details
#' **1. Overview**
#'
#' There is only one objective term, representing a linear score of the form
#' \eqn{a^\top x}, constructed by \code{objective_term()}.
#'
#' Key characteristics for the objective term:
#'
#' (1) Categorical or Quantitative
#'
#' \itemize{
#'   \item **maximize or minimize the number of items belonging to a specific
#'         categorical level**, or
#'   \item **maximize or minimize the sum of quantitative item-level attribute
#'         values**, such as difficulty, discrimination, or item information
#'         function values.
#' }
#'
#' (2) Module-, Pathway- or Panel-level
#'
#' \itemize{
#'   \item \strong{"Module-level"} — only items in a specified module contribute
#'         to the objective;
#'   \item \strong{"Pathway-level"} — items in all modules belonging to one
#'         pathway contribute; and
#'   \item \strong{"Panel-level"} — all item selections in the panel contribute.
#' }
#'
#' (3) Relative or Absolute Objective
#'
#' \itemize{
#'   \item \strong{Relative objective}: maximize/minimize \eqn{a^\top x}.
#'
#'   \item \strong{Absolute deviation objective}: minimize the deviation from
#'         a specified target: \eqn{ |a^\top x - \text{goal}|}
#' }
#'
#' Examples include:
#'
#' \itemize{
#'   \item *Relative objective* — “Maximize the number of items in category
#'         \eqn{c} selected in Module 1.”
#'   \item *Absolute objective* — “Minimize the deviation between the test
#'         information at \eqn{\theta = 0} and the target value 15.”
#' }
#'
#' @section Mathematical Formulation:
#'
#' Suppose the item pool contains S - 1 stimulus-based item sets, indexed by
#' s = 1, \ldots, S - 1. Each stimulus has a designated pivot item,
#' indexed by \eqn{i_s^{*}}. In addition, the pool contains a set of discrete
#' (non–stimulus-based) items, which can be represented by a dummy stimulus
#' s = S. Items belonging to stimulus s are indexed as
#' \eqn{i_s = 1, 2, \ldots, I_s}.
#'
#' Let \eqn{V_c^{item}} denote the set of
#' items that belong to category c and
#' \eqn{q_{i_s}} be the quantitative attribute value of item \eqn{i_s}.
#'
#' For absolute-deviation objectives, let \eqn{T} be the target value and
#' \eqn{d} be a non-negative deviation variable.
#'
#' Suppose there are M modules in an MST panel, Let \eqn{m = 1,\ldots,M}
#' denote the module id.
#'
#' ## **1. Module-level Objective (Module \eqn{m})**
#'
#' **(1a) Categorical attribute — relative objective**
#' \strong{Specification:} maximize or minimize the number of items from category \eqn{c} in module \eqn{m}.
#'
#' \deqn{
#'   \max/\min \quad
#'   \sum_{s=1}^S \sum_{i_s \in V_c^{item}} x_{m,i_s}.
#' }
#'
#' **(1b) Quantitative attribute — relative objective**
#' \strong{Specification:} maximize or minimize the sum of quantitative attribute values in module \eqn{m}.
#'
#' \deqn{
#'   \max/\min \quad
#'   \sum_{s=1}^S \sum_{i_s} q_{i_s} \, x_{m,i_s}.
#' }
#'
#' **(1c) Categorical attribute — absolute deviation objective**
#' \strong{Specification:} minimize deviation between the number of items from
#' category \eqn{c} in module \eqn{m} and the target value \eqn{T}.
#' \deqn{
#'   \min \; d
#' }
#'
#' subject to
#'
#' \deqn{
#'   \sum_{s=1}^S \sum_{i_s \in V_c^{item}} x_{m,i_s} - T \le d,
#' }
#' \deqn{
#'   T - \sum_{s=1}^S \sum_{i_s \in V_c^{item}} x_{m,i_s} \le d,
#' }
#'
#' **(1d) Quantitative attribute — absolute deviation objective**
#' \strong{Specification:} minimize deviation between the sum of attribute values
#' and the target value \eqn{T}.
#' \deqn{
#'   \min \; d
#' }
#'
#' subject to
#'
#' \deqn{
#'   \sum_{s=1}^S \sum_{i_s} q_{i_s} x_{m,i_s}- T \le d,
#' }
#' \deqn{
#'  T - \sum_{s=1}^S \sum_{i_s} q_{i_s} x_{m,i_s} \le d,
#' }
#'
#'
#' ## **2. Pathway-level Objective (Pathway \eqn{r})**
#'
#' Let \eqn{\mathcal{M}(r)} denote the set of modules belonging to pathway \eqn{r}.
#'
#' **(2a) Categorical attribute — relative objective**
#'
#' \deqn{
#'   \max/\min \quad
#'   \sum_{m \in \mathcal{M}(r)}
#'   \sum_{s=1}^S \sum_{i_s \in V_c^{item}} x_{m,i_s}.
#' }
#'
#' **(2b) Quantitative attribute — relative objective**
#'
#' \deqn{
#'   \max/\min \quad
#'   \sum_{m \in \mathcal{M}(r)}
#'   \sum_{s=1}^S \sum_{i_s} q_{i_s} x_{m,i_s}.
#' }
#'
#' **(2c) Categorical attribute — absolute deviation objective**
#'
#' \deqn{
#'   \min \; d
#' }
#'
#' subject to
#'
#' \deqn{
#'   \sum_{m \in \mathcal{M}(r)}\sum_{s=1}^S \sum_{i_s \in V_c^{item}} x_{m,i_s} - T \le d
#' }
#' \deqn{
#'  T - \sum_{m \in \mathcal{M}(r)}\sum_{s=1}^S \sum_{i_s \in V_c^{item}} x_{m,i_s} \le d
#' }
#'
#' **(2d) Quantitative attribute — absolute deviation objective**
#'
#' \deqn{
#'   \min \; d
#' }
#'
#' subject to
#'
#' \deqn{
#'   \sum_{m \in \mathcal{M}(r)} \sum_{s=1}^S \sum_{i_s} q_{i_s} x_{m,i_s} - T \le d,
#' }
#' \deqn{
#'   T -\sum_{m \in \mathcal{M}(r)} \sum_{s=1}^S \sum_{i_s} q_{i_s} x_{m,i_s} \le d,
#' }

#'
#'
#' ## **3. Panel-level Objective**
#'
#' **(3a) Categorical attribute — relative objective**
#'
#' \deqn{
#'   \max/\min \quad
#'   \sum_{m=1}^{M}
#'   \sum_{s=1}^S \sum_{i_s \in V_c^{item}} x_{m,i_s}.
#' }
#'
#' **(3b) Quantitative attribute — relative objective**
#'
#' \deqn{
#'   \max/\min \quad
#'   \sum_{m=1}^{M} \sum_{s=1}^S \sum_{i_s} q_{i_s} x_{m,i_s}.
#' }
#'
#' **(3c) Categorical attribute — absolute deviation objective**
#'
#' \deqn{
#'   \min \; d
#' }
#'
#' subject to
#'
#' \deqn{
#'   \sum_{m=1}^{M} \sum_{s=1}^S \sum_{i_s \in V_c^{item}} x_{m,i_s} - T \le d,
#' }
#' \deqn{
#'  T - \sum_{m=1}^{M} \sum_{s=1}^S \sum_{i_s \in V_c^{item}} x_{m,i_s} \le d,
#' }
#'
#' **(3d) Quantitative attribute — absolute deviation objective**
#'
#' \deqn{
#'   \min \; d
#' }
#'
#' subject to
#'
#' \deqn{
#'   \sum_{m=1}^{M}\sum_{s=1}^S \sum_{i_s} q_{i_s} x_{m,i_s} - T \le d,
#' }
#' \deqn{
#'   T - \sum_{m=1}^{M}\sum_{s=1}^S \sum_{i_s} q_{i_s} x_{m,i_s} \le d,
#' }
#'
#'
#'
#' @return A list of class \code{"compiled_objective"}
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
#'   \item{operators}{A character vector of constraint operators, one per row of `A_binary`.}
#'   \item{d}{A numeric vector of right-hand-side values for the constraints.}
#'   \item{C_binary}{A numeric vector of objective coefficients for binary decision
#'   variables.}
#'   \item{C_real}{A numeric vector of objective coefficients for continuous
#'   decision variables.}
#'   \item{sense}{"min" or "max"}
#'   \item{decisionvar_name_new}{Character vector indicating the names of new decision variables, same length as `ncol(A_real)`.}
#'   \item{decisionvar_type_new}{A character vector of `"C"` (continuous), same length as `ncol(A_real)`.}
#'   \item{notes}{A list containing the strategy name, strategy arguments, and \code{y_bounds}.}
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
#' # Example 1: relative objective for information at theta = 0 in the routing module
#' single_term <- objective_term(
#'   x             = test_mstATA,
#'   attribute     = "iif(theta=0)",
#'   cat_level         = NULL,
#'   applied_level = "Module-level",
#'   which_module  = 1,
#'   sense         = "max"
#' )
#' single_obj(x = test_mstATA, single_term = single_term)
#'
#' # Example 2: absolute objective for information at theta = 0 in the routing module
#' single_term <- objective_term(
#'   x             = test_mstATA,
#'   attribute     = "iif(theta=0)",
#'   cat_level         = NULL,
#'   applied_level = "Module-level",
#'   which_module  = 1,
#'   sense         = "min",
#'   goal = 10
#' )
#' single_obj(x = test_mstATA, single_term = single_term)
#' @export
#'
single_obj<-function(x,single_term){
  if (!inherits(x, "mstATA_design")) {
    stop("Input 'x' must be an object of class 'mstATA_design'.")
  }
  return(compile_objective(x = x,obj_set  = single_term,strategy = "single",strategy_args = list()))
}
