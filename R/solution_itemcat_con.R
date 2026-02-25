#' @title Generate Solution-Level Constraints for the Min/Exact/Max Number of Unique Items from Specific Categorical Levels
#'
#' @description
#' This function constructs **solution-level constraints** that regulate the number
#' of *unique items*—across all panels—that belong to specified categorical
#' levels.
#'
#' This constraint may enforce:
#'
#' \itemize{
#'   \item a minimum number of unique items in category c,
#'   \item a maximum number of unique items in category c, or
#'   \item an exact number of unique items in category c
#' }
#' across the entire multi-panel MST solution.
#'
#' Constraints are evaluated at the **Solution-level**, meaning that an item is
#' considered 'selected' if it appears in *any* module of *any* panel.
#'
#' The total number of constraints equals the number of categories in
#' \code{cat_levels}.
#'
#' @param x An object of class \code{"mstATA_design"} created by \code{mst_design()}.
#' @param attribute A string giving the column name in \code{x$ItemPool}
#'   containing the **item categorical attribute**.
#' @param cat_levels A character string or vector of category levels to constrain.
#'   These must be present in the unique values of the attribute column.
#' @param operator A character string specifying the constraint type.
#'   Must be one of \code{"<="}, \code{"="}, or \code{">="}.
#' @param target_num A scalar or vector giving the required number of unique
#'   items for each category level in \code{cat_levels}.
#'
#' @details
#' **1. Specification**
#'
#' The constraint enforces:
#'
#' \strong{
#' A minimum, exact, or maximum number of unique items from specified
#' categories must be selected across all assembled panels.
#' }
#'
#'
#' Key characteristics:
#' \itemize{
#'   \item The attribute is type is \emph{categorical}.
#'   \item The attribute is defined at the \emph{item level} in the item pool.
#'   \item The constraint is applied at the \strong{Solution-level}, i.e., across all panels.
#' }
#'
#' Users may limit the constraint to a subset of categories using \code{cat_levels}.
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
#' Let \eqn{V_c^{item}} denote the set of
#' items that belong to category c.
#'
#' \deqn{
#'   \sum_{s = 1}^{S} \sum_{i_s \in V_c^{item}} s_{i_s}
#'   \;\substack{\le \\ \ge \\ =}\;
#'   n_c^{item} .
#' }
#'
#' Here:
#' \itemize{
#'   \item \eqn{s_{i_s}} is the binary decision variable indicating whether
#'   item \eqn{i_s} is used at least once across all panels.
#'
#'   \item \eqn{\substack{\leq \\ \geq \\ =}} denotes that the specification may take the form of an
#'   upper bound, lower bound, or exact value.
#'
#'   \item \eqn{n_c^{item}} is the required number of unique item from category c
#'     across all panels.
#' }
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
#' @examples
#' data("mini_itempool")
#' test_mstATA <- mst_design(
#'   itempool      = mini_itempool,
#'   design        = "1-3-3",
#'   module_length = c(4,2,2,2,2,2,2)
#' )
#'
#' # Require at least 2 unique content1 items and at least 6 unique content3 items
#' solution_itemcat_con(
#'   x        = test_mstATA,
#'   attribute = "content",
#'   cat_levels    = c("content1", "content3"),
#'   operator  = ">=",
#'   target_num = c(2, 6)
#' )
#' @export

solution_itemcat_con<-function(x,attribute,cat_levels,operator,target_num){
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

  ItemPool<-x$ItemPool
  PoolSize<-nrow(x$ItemPool)

  target_num<-expand_target_to_matrix(input = target_num,levels = cat_levels,total_rows = 1,
                                      row_ids = 1)
  check_nonneg_integer(target_num,"target_num")
  category_vec<-check_attribute_column(itempool = ItemPool,attribute = attribute)
  NumCategory <- length(cat_levels)
  CategoryIndex <- vapply(seq_len(NumCategory),FUN = function(level_id) {
    get_attribute_val(ItemPool,attribute = attribute,cat_level = cat_levels[level_id]) },FUN.VALUE = integer(PoolSize))

  i_idx<-integer()
  j_idx<-integer()
  rhs<-numeric()
  ConstraintMatrix_name<-character(NumCategory)
  for(category_id in 1:NumCategory){
    item_idx<-which(CategoryIndex[,category_id]==1)
    i_idx<-c(i_idx,rep(category_id,length(item_idx)))
    j_idx<-c(j_idx,item_idx)
    rhs<-c(rhs,target_num[1,category_id])
    ConstraintMatrix_name[category_id]<-paste("The",name,"number of unique items from",cat_levels[category_id],"across panels")
  }
  Specification<-data.frame(a=paste0("Unique item count from ",paste(cat_levels,collapse = "/")),
                            b=attribute,c="Categorical",d="Solution-level",e=name,f=NumCategory,
                            stringsAsFactors = FALSE)
  colnames(Specification)<-c("Requirement","Attribute","Type","Application Level","Operator","Num of Constraints")
  Specification$`Num of Constraints`<-as.numeric(Specification$`Num of Constraints`)
  ConstraintMatrix<-Matrix::sparseMatrix(i = i_idx,j = j_idx,x = 1L,dims = c(NumCategory,PoolSize))
  colnames(ConstraintMatrix)<-paste0("s[", seq_len(PoolSize), "]")
  operators<-rep(operator,nrow(ConstraintMatrix))

  return(create_constraint(name = ConstraintMatrix_name,
                           specification = Specification,
                           A_binary=ConstraintMatrix,A_real = NULL,
                           operators=operators,d=rhs,
                           C_binary = NULL,C_real = NULL))
}
