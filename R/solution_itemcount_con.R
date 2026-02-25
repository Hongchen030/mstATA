#' @title Generate Solution-Level Constraints for the Number of Unique Items Selected Across Panels
#'
#' @description
#' Constructs solution-level constraints on the \emph{total number of distinct
#' items} selected across all panels in a multistage test (MST) assembly.
#'
#' This requirement enforces a user-specified:
#' \itemize{
#'   \item minimum number of unique items,
#'   \item maximum number of unique items, or
#'   \item exact (fixed) number of unique items
#' }
#' that may appear anywhere across all assembled panels.
#'
#' These constraints operate at the **solution level**, using decision variables
#' that indicate whether an item appears in \emph{any module of any panel}.
#'
#' The number of constraints generated is 1. If both a range needs to be specified,
#' call this function twice.
#'
#' @param x An object of class `"mstATA_design"` created by `mst_design()`.
#' @param operator A character string specifying the constraint type.
#'   Must be one of \code{"<="}, \code{"="}, or \code{">="}.
#' @param target_num A scalar giving the required number of unique
#'   items across panels.
#'
#' @details
#' **1. Specification**
#'
#' This constraint enforces:
#'
#' \strong{
#' A minimum, exact, or maximum number of \emph{unique items} must be selected
#' across all panels in one solution.
#' }
#'
#' Key characteristics:
#'
#' \itemize{
#'   \item The attribute type is \emph{categorical}: each item is assigned a unique ID.
#'   \item The attribute is defined at the \emph{item level} in the item pool.
#'   \item The constraint is applied at the \strong{Solution-level}, i.e., across all panels.
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
#' The solution-level variables track whether an item appears anywhere in the assembled
#' multi-panel solution.
#'
#' The total number of unique item selected at the solution
#' level is controlled through:
#'
#' \deqn{
#'   \sum_{s=1}^{S} \sum_{i_s=1}^{I_s} s_{i_s}
#'   \;\substack{\le \\ \ge \\ =}\;
#'   n^{item} .
#' }
#
#' Here:
#' \itemize{
#'   \item \eqn{s_{i_s}} is the binary decision variable indicating whether
#'   item \eqn{i_s} is used at least once across all panels.
#'
#'   \item \eqn{\substack{\leq \\ \geq \\ =}} denotes that the specification may take the form of an
#'   upper bound, lower bound, or exact value.
#'
#'   \item \eqn{n^{item}} is the user-specified minimum, exact, or maximum number of unique items.
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
#' @examples
#' data("mini_itempool")
#'
#' test_mstATA <- mst_design(
#'   itempool      = mini_itempool,
#'   design        = "1-3",
#'   module_length = c(3,4,4,4)
#' )
#'
#' # Require at least 20 unique items across all panels
#' solution_itemcount_con(test_mstATA, operator = ">=",target_num = 20)
#'
#' # Require at least 20 at most 30 unique items across all panels
#' con1<-solution_itemcount_con(test_mstATA, operator = ">=",target_num = 20)
#' con2<-solution_itemcount_con(test_mstATA, operator = "<=",target_num = 30)
#' @export



solution_itemcount_con<-function(x,operator, target_num){
  if (!inherits(x, "mstATA_design")) {
    stop("Input 'x' must be an object of class 'mstATA_design'.")
  }
  operator<-check_operator(operator = operator)
  if(operator==">="){
    name<-"(min)"
  }else if(operator=="<="){
    name<-"(max)"
  }else{
    name<-"(exact)"
  }
  ConstraintMatrix_name<-paste0(name," number of unique items across panels")
  check_nonneg_integer(target_num,"target_num")
  ItemPool<-x$ItemPool
  PoolSize<-nrow(ItemPool)
  ConstraintMatrix<-Matrix::Matrix(1, nrow = 1, ncol = PoolSize, sparse = TRUE)
  colnames(ConstraintMatrix)<-paste0("s[", seq_len(PoolSize), "]")

  Specification<-data.frame(a="Unique item count",
                            b="Item_id",c="Categorical",
                            d="Solution-level",e=name,f=1L,
                            stringsAsFactors = FALSE)
  colnames(Specification)<-c("Requirement","Attribute","Type","Application Level","Operator","Num of Constraints")
  Specification$`Num of Constraints`<-as.numeric(Specification$`Num of Constraints`)
  return(create_constraint(name=ConstraintMatrix_name,
                           specification = Specification,
                           A_binary = ConstraintMatrix,A_real = NULL,
                           operators = operator,d = target_num,
                           C_binary = NULL,C_real = NULL))
}
