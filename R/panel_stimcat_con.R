#' @title Generate Panel-Level Constraints for the Min/Exact/Max Number of Stimuli from Specific Categorical Levels
#'
#' @description
#' This function generates a linear constraint matrix based on a stimulus-level
#' categorical attribute (e.g., stimulus type, passage theme) across
#' all modules and pathways within a **single panel**.
#'
#' The constraint may enforce:
#' \itemize{
#'   \item a minimum number of stimuli in category c,
#'   \item a maximum number of stimuli in category c, or
#'   \item an exact (equality) requirement in category c.
#' }
#'
#'
#' The number of linear constraints generated equals the number of category
#' levels supplied in \code{levels}.
#'
#' @param x An object of class \code{"mstATA_design"} created by \code{mst_design()}.
#' @param attribute A string giving the column name in \code{x$ItemPool}
#'   containing the **stimulus-level categorical attribute**.
#' @param cat_levels A character vector of category levels to be constrained.
#'   Must match the unique values in the specified \code{attribute}.
#' @param operator A character string specifying the constraint direction:
#'   one of \code{"<="}, \code{"="}, or \code{">="}.
#' @param target_num A scalar or vector giving the required number of stimuli
#'   in each category listed in \code{cat_levels}.
#'
#' @details
#' **1. Specification**
#'
#' The constraint enforces:
#'
#' \strong{
#' A minimum, exact, or maximum number of stimuli from specified categories must
#' be selected somewhere within a panel.
#' }
#'
#' Key characteristics:
#' \itemize{
#'   \item The attribute type is \emph{categorical}.
#'   \item The attribute is defined at \emph{stimulus-level} in the item pool.
#'   \item The constraints are applied at the \emph{Panel-level}.
#' }
#'
#' Users may restrict the constraint to specific categories using the \code{cat_levels} argument.
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
#' Let \eqn{V_c^{stim}} denote the set of
#' stimuli that belong to category c.
#'
#' \deqn{
#'   \sum_{m} \sum_{s \in V_c^{stim}} x_{i_s^{*},m}
#'   \;\substack{\le \\ \ge \\ =}\;
#'   n_{c}^{stim}
#' }
#'
#' Here:
#' \itemize{
#'   \item \eqn{x_{i_s^{*},m}} indicates whether the pivot item for stimulus
#'   s is selected into module m, thereby indicating whether the
#'   stimulus s is selected in that module.
#'
#'   \item The stacked operator, \eqn{\substack{\leq \\ \geq \\ =}} denotes that the specification may take the form of an upper bound, lower bound, or exact value.
#'
#'   \item \eqn{n_c^{stim}} specify the required number of stimuli from category level c across modules/pathways in a panel.
#' }
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
#' data("reading_itempool")
#'
#' pivot_stim_map <- create_pivot_stimulus_map(
#'   itempool   = reading_itempool,
#'   stimulus   = "stimulus",
#'   pivot_item = "pivot_item"
#' )
#'
#' # Example: Require at most 4 history passages in an assembled panel
#' test_mstATA <- mst_design(
#'   itempool       = reading_itempool,
#'   design         = "1-3-3",
#'   module_length  = c(10,12,12,12,15,15,15),
#'   exclude_pathway = c("1-1-3", "1-3-1"),
#'   pivot_stim_map = pivot_stim_map
#' )
#'
#' panel_stimcat_con(
#'   x             = test_mstATA,
#'   attribute     = "stimulus_type",
#'   cat_levels        = "history",
#'   operator      = "<=",
#'   target_num    = 4
#' )
#' @export


panel_stimcat_con <- function(x,
                             attribute,cat_levels,
                             operator,target_num){

  if (!inherits(x, "mstATA_design")) {
    stop("Input 'x' must be an object of class 'mstATA_design'.")
  }
  pivot_stim_map<-x$pivot_stim_map
  if (is.null(pivot_stim_map)) {
    stop(
      "Stimulus-based constraints require `pivot_stim_map`. ",
      "Create it using `create_pivot_stimulus_map()` and supply it via `mst_design()`.",
      call. = FALSE
    )
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
  PoolSize<-nrow(ItemPool)
  NumModules <- x$NumModules
  num_decisions<-PoolSize*NumModules

  pivot_items<-pivot_stim_map$pivot_item_id
  category_vec<-check_attribute_column(itempool = ItemPool[pivot_items,],attribute = attribute)
  NumCategory <- length(cat_levels)
  CategoryIndex<-vapply(seq_len(NumCategory),FUN = function(level_id){
    get_attribute_val(ItemPool[pivot_items,],attribute = attribute,cat_level = cat_levels[level_id])
    },
    FUN.VALUE = integer(length(pivot_items)))

  target_num<-expand_target_to_matrix(input = target_num,levels = cat_levels,
                                      total_rows = 1L,row_ids = 1L)
  check_nonneg_integer(target_num,"target_num")
  num_constraints<-NumCategory
  i_idx<-integer()
  j_idx<-integer()
  rhs<-numeric(num_constraints)
  ConstraintMatrix_name<-character(num_constraints)
  col_offset<-(seq_len(NumModules)-1L)*PoolSize
  for(category_id in seq_len(NumCategory)){
    stim_idx<-pivot_items[which(CategoryIndex[,category_id]==1)]
    i_idx<-c(i_idx,rep(category_id,length(stim_idx)*NumModules))
    temp_j<-rep(stim_idx,NumModules)+rep(col_offset,each=length(stim_idx))
    j_idx<-c(j_idx, temp_j)
    rhs[category_id]<-target_num[1,category_id]
    ConstraintMatrix_name[category_id]<-paste("The",name,"number of stimuli from",cat_levels[category_id],"in a panel.")
  }
  Specification<-data.frame(a=paste0("Stimuli count from ",paste(cat_levels,collapse = "/")),
                            b=attribute,c="Categorical",d="Panel-level",e=name,f=num_constraints,
                            stringsAsFactors = FALSE)
  colnames(Specification)<-c("Requirement","Attribute","Type","Application Level","Operator","Num of Constraints")
  Specification$`Num of Constraints`<-as.numeric(Specification$`Num of Constraints`)

  ConstraintMatrix<-Matrix::sparseMatrix(i = i_idx,j = j_idx,x = 1L,dims = c(num_constraints,num_decisions))

  decisionvar_name<-x$decisionvar_name
  colnames(ConstraintMatrix)<-paste0("x[", rep(seq_len(NumModules), each = PoolSize), ",", rep(seq_len(PoolSize), NumModules), "]")
  if(length(decisionvar_name)!=num_decisions){
    ConstraintMatrix<-ConstraintMatrix[,decisionvar_name]
  }

  return(create_constraint(name = ConstraintMatrix_name,
                           specification = Specification,
                           A_binary=ConstraintMatrix,A_real = NULL,
                           operators=rep(operator,num_constraints),d=rhs,
                           C_binary = NULL,C_real = NULL))

}
