#' @title Generate Module- or Pathway-Level Range Constraints for the Number of
#' Items from Specific Categorical Levels
#'
#' @description
#' This function generates constraint matrices that enforce lower and upper bounds
#' on the number of selected items belonging to specific categorical attribute
#' levels (e.g., content area, item format) within modules or pathways.
#'
#' It supports two specification formats:
#' \itemize{
#'   \item \strong{Direct range constraints:} \code{min} and/or \code{max}
#'   \item \strong{Target ± deviation constraints:} \code{target} and \code{deviation}
#' }
#'
#' Constraints may apply at the:
#' \itemize{
#'   \item \strong{"Module-level"} — each module is treated as an independent test form
#'   \item \strong{"Pathway-level"} — each pathway is treated as an independent test form
#' }
#'
#' The total number of constraints depends on the application level and the
#' number of category levels in \code{cat_levels}:
#'
#' \itemize{
#'   \item \strong{Module-level:} when \code{which_module} is provided,
#'         or both \code{which_module} and \code{which_pathway} are \code{NULL}
#'
#'    The number of constraints =
#'      \strong{(number of category levels) × (number of modules specified) × side}
#'
#'   \item \strong{Pathway-level:} when \code{which_pathway} is provided and \code{which_module = NULL}
#'
#'    The number of constraints =
#'      \strong{(number of category levels) × (number of pathways specified) × side}
#' }
#'
#' Here, \code{side = 2} for range constraints (lower + upper bound) and
#' \code{side = 1} when only one bound is active.
#'
#'
#' @param x An object of class \code{"mstATA_design"} created by \code{mst_design()}.
#' @param attribute A string giving the column name in \code{x$ItemPool} that contains
#'   the **item categorical attribute**.
#' @param cat_levels Character vector of category levels to be constrained.
#' @param min Optional minimum number of items (scalar, vector, or matrix).
#' @param max Optional maximum number of items (scalar, vector, or matrix).
#' @param target Optional target number of items (scalar, vector, or matrix).
#' @param deviation Optional allowable deviation from \code{target}
#'   (scalar, vector, or matrix).
#' @param which_module Optional integer vector of module indices to which the constraints
#'   apply.
#' @param which_pathway Optional integer vector of pathway indices to which the
#'   constraints apply.
#'
#' @details
#' **1. Specification**
#'
#' The constraint enforces:
#'
#' \strong{The number of selected items in each specified category must fall within
#' an allowable range.}
#'
#' Key characteristics:
#'
#' \itemize{
#'   \item The attribute type is \emph{categorical}.
#'   \item The attribute is defined at the \emph{item level} in the item pool.
#'   \item The constraints are applied at either \strong{"Module-level"} or \strong{"Pathway-level"}.
#' }
#'
#' **2. Range specification: min, max, target, deviation**
#'
#' Each of the four range-specification arguments may be:
#' \code{NULL}, scalar, vector, or matrix.
#'
#' Interpretation:
#' \itemize{
#'   \item \code{min}: minimum number of items per category per module/pathway
#'   \item \code{max}: maximum number of items per category per module/pathway
#'   \item \code{target}: desired number of items per category per module/pathway
#'   \item \code{deviation}: allowable deviation from \code{target}
#' }
#'
#' When only \code{target} and \code{deviation} are provided:
#'
#' \deqn{
#'   \text{min} = \text{target} - \text{deviation}, \qquad
#'   \text{max} = \text{target} + \text{deviation}.
#' }
#'
#' @section Mathematical Formulation:
#'
#' Suppose the item pool contains (S - 1) stimulus-based item sets, indexed by
#' \eqn{s = 1, \ldots, S - 1}. Each stimulus has a designated pivot item,
#' indexed by \eqn{i_s^{*}}. In addition, the pool contains a set of discrete
#' (non–stimulus-based) items, which are represented by a dummy stimulus
#' \eqn{s = S} to allow a unified indexing scheme. Items belonging to stimulus
#' \eqn{s} are indexed as \eqn{i_s = 1, 2, \ldots, I_s}.
#'
#' Suppose there are \eqn{M} modules in an MST panel. Let
#' \eqn{m = 1, \ldots, M} denote the module index.
#'
#' Let \eqn{V_c^{item}} denote the set of
#' items that belong to category c.
#'
#' **1. Module-level constraint (for module m)**
#' \deqn{
#'   n_c^{m,\min}
#'   \;\le\;
#'    \sum_{s=1}^{S} \sum_{i_s \in V_c^{item}} x_{i_s,m}
#'   \;\le\;
#'   n_c^{m,\max}
#' }
#'
#'
#' **2. Pathway-level constraint (for pathway r)**
#'
#' \deqn{
#'   n_c^{r,\min}
#'   \;\le\;
#'    \sum_{m \in r} \sum_{s=1}^{S} \sum_{i_s \in V_c^{item}} x_{i_s,m}
#'   \;\le\;
#'   n_c^{r,\max}
#' }
#'
#'
#' Here:
#' \itemize{
#'   \item \eqn{x_{i_s,m}} is the binary decision variable indicating whether
#'   item \eqn{i_s} is selected into module m.
#'
#'   \item \eqn{m \in r} denote modules belonging to pathway r.
#'
#'   \item \eqn{n_c^{m,\min}} and \eqn{n_c^{m,\max}} specify the minimum and maximum
#'   required number of items from category level c in module m.
#'
#'   \item \eqn{n_c^{r,\min}} and \eqn{n_c^{r,\max}} specify the minimum and maximum
#'   required number of items from category level c in pathway r.
#' }
#'
#' @return An object of S3 class \code{"mstATA_constraint"} with named elements:
#' \describe{
#'   \item{name}{A string indicating the specification name}
#'   \item{specification}{A \code{data.frame} summarizing the constraint specification, including
#'    the requirement name, attribute, constraint type, application level,
#'    operator, and the number of constraint rows generated.}
#'   \item{A_binary}{A sparse binary matrix representing the linear constraint coefficients.}
#'   \item{A_real}{NULL for 'mstATA_constraint' object}
#'   \item{operators}{A character vector of constraint operators, one per row of `A_binary`.}
#'   \item{d}{A numeric vector of right-hand-side values for the constraints.}
#'   \item{C_binary}{NULL for 'mstATA_constraint' object}
#'   \item{C_real}{NULL for 'mstATA_constraint' object}
#'   \item{sense}{NULL for 'mstATA_constraint' object}
#' }
#' @seealso [test_itemcat_con()]
#' @examples
#' data("mini_itempool")
#' # Example 1: same category ranges across all modules
#' # MST 1-3-3 design, 7 modules, 2 categories (MC, TEI).
#' test_mstATA <- mst_design(
#'   itempool = mini_itempool,
#'   design = "1-3-3",
#'   module_length = c(4,3,3,3,4,4,4)
#' )
#'
#' test_itemcat_range_con(
#'   x = test_mstATA,
#'   attribute = "itemtype",
#'   cat_levels = c("MC","TEI"),
#'   min = matrix(c(2,1, rep(c(1,0),3), rep(c(1,0),3)), ncol = 2, byrow = TRUE),
#'   max = matrix(c(3,3, rep(c(6,1),3), rep(c(7,1),3)), ncol = 2, byrow = TRUE),
#'   target = NULL,
#'   deviation = NULL
#' )
#'
#'
#' # Example 2: different category ranges for each module
#' test_mstATA <- mst_design(
#'   itempool = mini_itempool,
#'   design = "1-3-3",
#'   module_length = c(4,3,3,3,4,4,4)
#' )
#'
#' test_itemcat_range_con(
#'   x = test_mstATA,
#'   attribute = "itemtype",
#'   cat_levels = c("MC","TEI"),
#'   min = matrix(c(2,1, 1,0, 2,0, 1,0, 2,0, 3,1, 2,0),
#'                ncol = 2, byrow = TRUE),
#'   max = matrix(c(3,3, 2,2, 3,2, 3,1, 4,2, 4,2, 4,2),
#'                ncol = 2, byrow = TRUE),
#'   which_module = 1:7
#' )
#'
#'
#' # Example 3: same category range per pathway (target ± deviation)
#' test_mstATA <- mst_design(
#'   itempool = mini_itempool,
#'   design = "1-3-3",
#'   module_length = c(4,3,3,3,4,4,4),
#'   exclude_pathways = c("1-1-3","1-3-1")
#' )
#'
#' test_itemcat_range_con(
#'   x = test_mstATA,
#'   attribute = "itemtype",
#'   cat_levels = c("MC","TEI"),
#'   target  = matrix(c(9,2), nrow = 7, ncol = 2, byrow = TRUE),
#'   deviation = matrix(c(1,1), nrow = 7, ncol = 2, byrow = TRUE),
#'   which_pathway = 1:7
#' )
#'
#' @export

test_itemcat_range_con <- function(x,attribute, cat_levels,
                                   min = NULL,max = NULL,
                                   target = NULL,deviation = NULL,
                                   which_module = NULL,which_pathway = NULL) {

  if (!inherits(x, "mstATA_design")) {
    stop("Input 'x' must be an object of class 'mstATA_design'.")
  }
  NumModules <- x$NumModules
  NumPathways<-x$NumPathways
  check_scope<-check_test_constraint_scope(num_modules = NumModules,num_pathways = NumPathways,
                                           which_module = which_module, which_pathway = which_pathway)
  which_module <- check_scope[["which_module"]]
  which_pathway <- check_scope[["which_pathway"]]
  total_rows<-check_scope[["total_rows"]]
  row_ids<-check_scope[["row_ids"]]

  # Detect user intent
  has_min  <- !is.null(min)
  has_max  <- !is.null(max)
  has_targ <- !is.null(target) && !is.null(deviation)

  if (xor(has_min, has_max) && !has_targ) {
    op  <- if (has_min) ">=" else "<="
    val <- if (has_min) min   else max
    val_matrix <-expand_target_to_matrix(input = val,levels = cat_levels,total_rows = total_rows,row_ids = row_ids)
    check_nonneg_integer(val_matrix,"input number")
    return(test_itemcat_con(x = x,attribute = attribute,cat_levels = cat_levels,
                            operator = op,target_num = val_matrix,
                            which_module = which_module,
                            which_pathway = which_pathway))
  }

  constraints<-list()
  bounds <- check_target_bounds(levels = cat_levels,min = min,max = max,target = target,deviation = deviation,
                                total_rows = total_rows,row_ids = row_ids)

  constraints[[1]] <- test_itemcat_con(x = x,attribute = attribute,cat_levels  = cat_levels,
                                                   operator = ">=",target_num = bounds$min,
                                                   which_module = which_module,
                                                   which_pathway = which_pathway)
  constraints[[2]] <- test_itemcat_con(x = x,attribute = attribute,cat_levels  = cat_levels,
                                                   operator = "<=",target_num = bounds$max,
                                                   which_module = which_module,
                                                   which_pathway = which_pathway)
  combined<-combine_constraints(constraints)
  return(combined)
}
