#' @title Defining Solution-level Item Indicator Variables and Generating Constraints for Item Exposure Control Across Multiple Panels
#'
#' @description
#' This function does **not** need to be called by users and is internally used by `multipanel_spec()`.
#' It is a **structural component** of multi-panel MST assembly.
#'
#' It has two roles.
#'
#' - Define the semantics of the solution-level indicator
#' variable \eqn{s_i}: an item is considered *used* if and only if it is selected
#' in at least one module of at least one panel.
#'
#' - Constrain user-specified **minimum and maximum
#' reuse limits** for each item across all panels.
#' The total number of constraints depends on the number of items in the item pool
#' (for min and max usage, two constraints per item).
#'
#' @param x An object of class \code{"mstATA_design"} created by \code{mst_design()}.
#' @param num_panels Integer. Number of panels to be assembled.
#' @param global_min_use Scalar. Minimum number of times any item may be used
#'   across panels. Default = \code{0}.
#' @param global_max_use Scalar. Maximum number of times any item may be used
#'   across panels. Default = \code{Inf}.
#' @param item_min_use Optional data frame with columns \code{item_id} and
#'   \code{min}, specifying item-specific minimum reuse counts. Default is \code{NULL}.
#' @param item_max_use Optional data frame with columns \code{item_id} and
#'   \code{max}, specifying item-specific maximum reuse counts. Default is \code{NULL}.
#'
#' @details
#'
#' **1. Specification**
#'
#' The constraint enforces:
#'
#' \strong{
#' Each item can be used at least a specified minimum number of times and/or
#' no more than a specified maximum number of times across all panels.
#' }
#'
#' Key characteristics:
#'
#' \itemize{
#'   \item Attribute: \emph{itemset level (item-itself set)} \emph{logical} (if selected, then increment reuse).
#'   \item Constraints are applied at the \strong{"Solution-level"}.
#' }
#'
#'
#' **2. Number of times an item can be selected**
#'
#' Global reuse limits are supplied via \code{global_min_use} and
#' \code{global_max_use}. These bounds apply to all items unless overridden.
#'
#' Item-specific reuse limits may be supplied through:
#' \itemize{
#'   \item \code{item_min_use}: a data frame with columns \code{item_id} and \code{min}
#'   \item \code{item_max_use}: a data frame with columns \code{item_id} and \code{max}
#' }
#'
#'
#' The total number of constraints generated is always
#' \eqn{2 \times \text{PoolSize}}, corresponding to one minimum and one maximum
#' constraint per item.
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
#' \eqn{m = 1, \ldots, M} denote the module index. Suppose
#' there are \eqn{P} panels assembled in one solution. Let
#' \eqn{p = 1, \ldots, P} denote the panel index.
#'
#' **Decision variable set 1: item-module-panel selection variables**
#'
#' For each item \eqn{i_s},
#'
#' \deqn{
#'   x_{i_s,m,p} =
#'   \begin{cases}
#'     1, & \text{if item } i_s \text{ is selected in module } m \text{ of panel } p, \\
#'     0, & \text{otherwise}.
#'   \end{cases}
#' }
#'
#' **Decision variable set 2: solution-level item indicators**
#'
#' For each item \eqn{i_s},
#'
#' \deqn{
#'   s_{i_s} =
#'   \begin{cases}
#'     1, & \text{if item } i_s \text{ is used in any module in any panel}, \\
#'     0, & \text{otherwise}.
#'   \end{cases}
#' }
#'
#' **Number of times item i is selected in a solution**
#'
#' \deqn{
#'   u_i = \sum_{p}\sum_{m} x_{i_s,m,p}.
#' }
#'
#' This constraints enforce:
#'
#' \deqn{
#'   \text{min\_use}_i \, s_{i_s} \le u_i \le \text{max\_use}_i \, s_{i_s},
#' }
#'
#' ensuring consistency between panel-level selections and solution-level
#' indicators while enforcing reuse limits.
#'
#' where:
#' \itemize{
#'   \item \eqn{x_{i_s,m,p}} indicates whether item \eqn{i_s} is selected in module
#'         \eqn{m} of panel \eqn{p};
#'   \item \eqn{s_{i_s}} indicates whether item \eqn{i_s} is used anywhere in the solution;
#'   \item \eqn{\text{min\_use}_i} and \eqn{\text{max\_use}_i} are item-specific
#'         reuse bounds.
#' }
#'
#' These constraints are **structural**: they define the meaning of the
#' solution-level indicator variables and also create constraints for the
#' item exposure control across panels.
#'
#' \itemize{
#'   \item If \eqn{s_i = 0}, then item \eqn{i} cannot be selected in any module
#'         of any panel.
#'   \item If \eqn{s_i = 1}, the total number of selections of item \eqn{i}
#'         must lie within the specified reuse bounds.
#'   \item If item \eqn{i} is not selected in any module across panels,
#'         then \eqn{s_i} must equal 0.
#'   \item If item \eqn{i} is selected in at least one module of any panel,
#'         then \eqn{s_i} must equal 1.
#' }
#'
#'
#'
#' @return An object of S3 class \code{"mstATA_constraint"} with named elements:
#' \describe{
#'   \item{name}{A character vector indicating the specifications in each row of `A_binary`}
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
#'
#'
#' @examples
#' data("mini_itempool")
#'
#' test_mstATA <- mst_design(
#'   itempool      = mini_itempool,
#'   design        = "1-3-3",
#'   module_length = c(6, 4, 4, 4, 3, 3, 3)
#' )
#'
#' # Example: Allow each item to be used at most once across all panels
#' dvlink_item_solution(
#'   x = test_mstATA,
#'   num_panels = 2,
#'   global_max_use = 1
#' )
#'
#'
#' @export

dvlink_item_solution <- function(x,num_panels,
                                 global_min_use = 0, global_max_use = Inf,
                                 item_min_use = NULL,item_max_use = NULL) {
  if (!inherits(x, "mstATA_design")) {
    stop("Input x must be an object of class 'mstATA_design'.")
  }

  if (!is.numeric(num_panels) || length(num_panels) != 1L ||
      num_panels < 1 || num_panels != as.integer(num_panels)) {
    stop("`num_panels` must be a positive integer.", call. = FALSE)
  }


  ItemPool<-x$ItemPool
  PoolSize <- nrow(ItemPool)
  item_id_col<-x$item_id_col
  index_map <- setNames(seq_len(PoolSize), ItemPool[[item_id_col]])
  NumModules<-x$NumModules

  decisionvar_name<-x$decisionvar_name
  block_size<-length(decisionvar_name)
  item_x_cols <- expand_itemdv_panels(x,num_panels)
  num_x_imp<-block_size*num_panels
  if(!is.finite(global_max_use) || global_max_use>= NumModules*num_panels){
    global_max_use<-NumModules*num_panels
  }
  min_use <- rep(global_min_use, PoolSize)
  max_use <- rep(global_max_use, PoolSize)

  if (!is.null(item_min_use)) {
    if (!all(c("item_id", "min") %in% names(item_min_use))) {
      stop("`item_min_use` must contain columns 'item_id' and 'min'.", call. = FALSE)
    }

    idx<-check_item_ids(item_ids = item_min_use$item_id,item_names = names(index_map))
    min_use[idx] <- item_min_use$min
  }
  if (!is.null(item_max_use)) {
    if (!all(c("item_id", "max") %in% names(item_max_use))) {
      stop("`item_max_use` must contain columns 'item_id' and 'max'.", call. = FALSE)
    }
    idx<-check_item_ids(item_ids = item_max_use$item_id,item_names = names(index_map))
    max_use[idx] <- item_max_use$max
  }
  if (any(min_use > max_use)) stop("For at least one item, min_use > max_use.", call.=FALSE)
  if (any(min_use < 0, na.rm = TRUE)) stop("min_use must be >= 0.", call. = FALSE)
  if (any(max_use < 0, na.rm = TRUE)) stop("max_use must be >= 0.", call. = FALSE)

  min_use[min_use<1]<-1

  min_constraints<-max_constraints<-PoolSize
  num_constraints <- min_constraints+max_constraints

  x_names_multi <- expand_binary_dv_to_panels(decisionvar_name, num_panels)
  s_names <- paste0("s[", seq_len(PoolSize), "]")

  ConstraintMatrix <- Matrix::Matrix(0, nrow = num_constraints,
                                     ncol = num_x_imp+PoolSize,
                                     sparse = TRUE)
  colnames(ConstraintMatrix)<-c(x_names_multi,s_names)
  operators <- rep(c(">=","<="),each = PoolSize)
  rhs <- numeric(length = num_constraints)
  ConstraintMatrix_name <- character(num_constraints)
  for (i in seq_len(PoolSize)) {
    row_min<-i
    row_max<-PoolSize+i
    cols_i <- item_x_cols[[i]]
    ConstraintMatrix[row_min, cols_i] <- 1
    ConstraintMatrix[row_max, cols_i] <- 1
    ConstraintMatrix[row_min,(num_x_imp+i)]<- -min_use[i]
    ConstraintMatrix[row_max,(num_x_imp+i)]<- -max_use[i]
    ConstraintMatrix_name[c(row_min,row_max)] <- c(sprintf("Min exposure for item %d", i),
                                                   sprintf("Max exposure for item %d", i))
  }

  Specification <- data.frame(Requirement = "Item exposure control across panels",
                              Attribute = "Item_id (item-itself set)",
                              Type = "Logical",
                              `Application Level` = "Solution-level",
                              Operator =  "(range)",f=num_constraints,
                              stringsAsFactors = FALSE)
  names(Specification) <- c("Requirement","Attribute","Type","Application Level","Operator","Num of Constraints")
  Specification$`Num of Constraints`<-as.numeric(Specification$`Num of Constraints`)

  return(create_constraint(name = ConstraintMatrix_name,specification = Specification,
                           A_binary = ConstraintMatrix,A_real = NULL,
                           operators = operators,d = rhs,
                           C_binary = NULL,C_real = NULL))
}

