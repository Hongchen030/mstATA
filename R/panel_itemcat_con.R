#' @title Generate Panel-Level Constraints for the Min/Exact/Max Number of Items
#'   from Specific Categorical Levels
#'
#' @description
#' This function generates a linear constraint matrix that restricts the number
#' of items belonging to specified categorical levels (e.g., content area,
#' format type) across all modules/pathways in an MST panel.
#'
#' The constraint may enforce:
#' \itemize{
#'   \item a minimum number of items in category c,
#'   \item a maximum number of items in category c, or
#'   \item an exact (equality) requirement for category c.
#' }
#'
#' The total number of constraints equals the number of category levels supplied
#' in \code{cat_levels}.
#'
#'
#' @param x An object of class \code{"mstATA_design"} created by \code{mst_design()}.
#' @param attribute A string giving the column name in \code{x$ItemPool} that contains
#'   the **item categorical attribute**.
#' @param cat_levels A character string or vector of categorical attribute levels.
#'   These define the item categories being constrained.
#' @param operator A character string indicating the type of constraint.
#'   Must be one of \code{"<="}, \code{"="}, or \code{">="}.
#' @param target_num A scalar or vector specifying the required number of items
#'   for each category in \code{cat_levels}.
#'
#' @details
#' **1. Specification**
#'
#' The constraint enforces:
#'
#' \strong{
#' A minimum, exact, or maximum number of items from specified categorical
#' levels must be selected within a panel.
#' }
#'
#'
#' Key characteristics:
#' \itemize{
#'   \item The attribute type is \emph{categorical}.
#'   \item The attribute is defined at the \emph{item level} in the item pool.
#'   \item The constraints are applied at the \emph{Panel-level}.
#' }
#'
#' Users may restrict the constraints to any subset of categories via the
#' \code{cat_levels} argument.
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
#' \deqn{
#'   \sum_{m} \sum_{s = 1}^{S} \sum_{i_s \in V_c^{item}} x_{i_s,m}
#'   \;\substack{\le \\ \ge \\ =}\;
#'   n_{c}^{item} ,\quad s = 1, \ldots, S,\; i_s = 1, \ldots, I_s.
#' }
#'
#' Here:
#' \itemize{
#'   \item \eqn{x_{i_s,m}} is the binary decision variable indicating whether
#'   item \eqn{i_s} is selected into module m.
#'
#'   \item The stacked operator, \eqn{\substack{\leq \\ \geq \\ =}} denotes that the specification may take the form of an upper bound, lower bound, or exact value.
#'
#'   \item \eqn{n_c^{item}} specify the required number of items from category level c in a panel.
#' }
#'
#' @return An object of S3 class \code{"constraint"} with named elements:
#' \describe{
#'   \item{name}{A character vector indicating the specifications in each row of `A_binary`}
#'   \item{specification}{A \code{data.frame} summarizing the constraint specification, including
#'    the requirement name, attribute, constraint type, application level,
#'    operator, and the number of constraint rows generated.}
#'   \item{A_binary}{A sparse binary matrix representing the linear constraint coefficients.}
#'   \item{C_binary}{NULL for 'mstATA_constraint' object}
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
#'   design        = "1-3-3",
#'   module_length = c(4,2,2,2,2,2,2)
#' )
#'
#' # Example 1: Require at least 10 multiple-choice items in the panel
#' panel_itemcat_con(
#'   x         = test_mstATA,
#'   attribute = "itemtype",
#'   cat_levels    = "MC",
#'   operator  = ">=",
#'   target_num = 10
#' )
#'
#' # Example 2: Require at least 10 multiple-choice items, at least 3 TEI items in the panel
#' panel_itemcat_con(
#'   x         = test_mstATA,
#'   attribute = "itemtype",
#'   cat_levels    = c("MC","TEI"),
#'   operator  = ">=",
#'   target_num = c(10,5)
#' )
#' @export


panel_itemcat_con<-function(x,attribute,cat_levels,operator,target_num){
  if (!inherits(x, "mstATA_design")) {
    stop("Input 'x' must be an object of class 'mstATA_design'.")
  }
  operator<-check_operator(operator= operator)
  if(operator==">="){
    name<-"(min)"
  }else if(operator=="<="){
    name<-"(max)"
  }else{
    name<-"(exact)"
  }

  ItemPool<-x$ItemPool
  PoolSize<-nrow(x$ItemPool)
  NumModules <- x$NumModules
  num_decisions<-PoolSize*NumModules

  target_num<-expand_target_to_matrix(input = target_num,levels = cat_levels,total_rows = 1L,
                                      row_ids = 1L)
  check_nonneg_integer(target_num,"target_num")

  category_vec<-check_attribute_column(itempool = ItemPool,attribute = attribute)
  NumCategory <- length(cat_levels)
  CategoryIndex <- vapply(seq_len(NumCategory),FUN = function(level_id) {
    get_attribute_val(ItemPool,attribute = attribute,cat_level = cat_levels[level_id]) },
    FUN.VALUE = integer(PoolSize))

  i_idx<-integer()
  j_idx<-integer()
  num_constraints<-NumCategory
  ConstraintMatrix_name<-character(num_constraints)
  rhs<-numeric(num_constraints)
  col_offset<-(seq_len(NumModules)-1L)*PoolSize
  for(category_id in 1:NumCategory){
    item_idx<-which(CategoryIndex[,category_id]==1)
    i_idx<-c(i_idx,rep(category_id,length(item_idx)*NumModules))
    temp_j<-rep(item_idx,NumModules)+rep(col_offset,each=length(item_idx))
    j_idx<-c(j_idx,temp_j)
    ConstraintMatrix_name[category_id]<-paste("The",name,"number of items from",cat_levels[category_id],"in a panel")
    rhs[category_id]<-target_num[1,category_id]
  }
  Specification<-data.frame(a=paste0(paste(cat_levels,collapse = "/")," item count"),
                            b=attribute,c="Categorical",d="Panel-level",e=name,f=NumCategory,
                            stringsAsFactors = FALSE)
  colnames(Specification)<-c("Requirement","Attribute","Type","Application Level","Operator","Num of Constraints")
  Specification$`Num of Constraints`<-as.numeric(Specification$`Num of Constraints`)
  ConstraintMatrix<-Matrix::sparseMatrix(i = i_idx,j = j_idx,x = 1L,dims = c(num_constraints,num_decisions))
  operators<-rep(operator,nrow(ConstraintMatrix))
  decisionvar_name<-x$decisionvar_name
  colnames(ConstraintMatrix)<- paste0("x[", rep(seq_len(NumModules), each = PoolSize), ",", rep(seq_len(PoolSize), NumModules), "]")
  if(length(decisionvar_name)!=num_decisions){
    ConstraintMatrix<-ConstraintMatrix[,decisionvar_name,drop=FALSE]
  }
  return(create_constraint(name = ConstraintMatrix_name,
                           specification = Specification,
                           A_binary=ConstraintMatrix,A_real = NULL,
                           operators=operators,d=rhs,
                           C_binary = NULL,C_real = NULL))
}
