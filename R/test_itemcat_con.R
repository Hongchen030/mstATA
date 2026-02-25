#' @title Generate Module- or Pathway-Level Constraints for the Minimum, Exact,
#' or Maximum Number of Items from Specific Categorical Levels
#'
#' @description
#' This function generates a linear constraint matrix enforcing a minimum,
#' exact, or maximum number of items from specified categorical
#' levels (e.g., content area, item format).
#'
#' Constraints may apply at the:
#' \itemize{
#'   \item \strong{"Module-level"} — each module is treated as an independent test form
#'   \item \strong{"Pathway-level"} — each pathway is treated as an independent test form
#' }
#'
#' The total number of constraints depends on the application level and the
#' number of category levels specified in \code{cat_levels}:
#'
#' \itemize{
#'   \item \strong{Module-level:} when \code{which_module} is provided
#'          OR both \code{which_module} and \code{which_pathway} are \code{NULL}
#'
#'    The number of constraints is
#'     \strong{(number of category levels)× (number of modules specified)}
#'
#'   \item \strong{Pathway-level:} when \code{which_pathway} is provided and
#'         \code{which_module = NULL}
#'
#'   The number of constraints is
#'     \strong{(number of category levels) × (number of pathways specified)}
#' }
#'
#' Users may constrain a subset of modules or pathways using the
#' \code{which_module} or \code{which_pathway} arguments, and may restrict the
#' constraint to specific categories using \code{cat_levels}.
#'
#' @param x An object of class \code{"mstATA_design"} created by \code{mst_design()}.
#' @param attribute A string giving the column name in \code{x$ItemPool} that
#'   contains the **item categorical attribute**.
#' @param cat_levels A character string or vector of categorical levels to be
#'   constrained.
#' @param operator A character string specifying the inequality or equality
#'   operator. Must be one of \code{"<="}, \code{"="}, or \code{">="}.
#'   For range-based constraints, use \code{test_itemcat_range_con()} instead.
#' @param target_num A matrix or scalar/vector specifying the required number of items for
#' the category levels in \code{cat_levels}. If scalar/vector is provided, it expands to a matrix.
#'   See **Details**.
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
#' \strong{The test must meet a minimum, exact, or maximum number of items from the specified categories.}
#'
#' Key characteristics:
#'
#' \itemize{
#'   \item The attribute type is \emph{categorical}.
#'   \item The attribute is defined at the \emph{item level} in the item pool.
#'   \item The constraints are applied at either \strong{"Module-level"} or \strong{"Pathway-level"}.
#' }
#'
#' **2. target_num**
#'
#' The constraint level is determined by which selection argument is provided:
#'
#' \itemize{
#'
#'   \item \strong{Module-level}:
#'
#'   - Requires the minimum/exact/maximum number of items from specified category levels
#'       to be selected in specified module.
#'   - Expanded \code{target_num} matrix:
#'       each row = one specified module, each column = one specified category.
#'
#'
#'   \item \strong{Pathway-level}:
#'
#'  - Requires the minimum/exact/maximum number of items from specified category levels
#'       to be selected across the modules belonging to specified pathway.
#'  - Expanded \code{target_num} matrix:
#'       each row = one specified pathway, each column = one specified category.
#'
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
#'
#' \deqn{\sum_{s=1}^{S} \sum_{i_s \in V_c^{item}} x_{i_s,m} \;\substack{\le \\ \ge \\ =}\; n_c^{m}}
#'
#' **2. Pathway-level constraint (for pathway r)**
#'
#' \deqn{\sum_{m \in r} \sum_{s=1}^{S} \sum_{i_s \in V_c^{item}} x_{i_s,m} \;\substack{\le \\ \ge \\ =}\; n_c^{r}}
#'
#' Here:
#' \itemize{
#'   \item \eqn{x_{i_s,m}} is the binary decision variable indicating whether
#'   item \eqn{i_s} is selected into module m.
#'
#'   \item \eqn{m \in r} denote modules belonging to pathway r.
#'
#'   \item The stacked operator, \eqn{\substack{\leq \\ \geq \\ =}} denotes that the specification may take the form of an upper bound, lower bound, or exact value.
#'
#'   \item \eqn{n_c^{m}} specify the required number of items from category level c in module m.
#'
#'   \item \eqn{n_c^{r}} specify the required number of items from category level c in pathway r.
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
#'
#' @examples
#' data("mini_itempool")
#'
#' test_mstATA <- mst_design(
#'   itempool = mini_itempool,
#'   design = "1-3-3",
#'   module_length = c(4,2,2,2,2,2,2)
#' )
#' # Example 1: category constraints across modules in the same stage
#' con1<-test_itemcat_con(x = test_mstATA,attribute = "itemtype",cat_levels = c("MC","TEI"),
#'                                        operator = "=",target_num = c(3,1),
#'                                        which_module = 1,which_pathway = NULL)
#' con2<-test_itemcat_con(x = test_mstATA,attribute = "itemtype",cat_levels = c("MC","TEI"),
#'                                        operator = "=",target_num = c(2,0),
#'                                        which_module = 2:4,which_pathway = NULL)
#' con3<-test_itemcat_con(x = test_mstATA,attribute = "itemtype",cat_levels = c("MC","TEI"),
#'                                        operator = "=",target_num = c(2,0),
#'                                        which_module = 5:7,which_pathway = NULL)
#' # Example 2: category constraints per pathway
#' test_mstATA<-mst_design(itempool = mini_itempool,design = "1-3-3",
#'                           exclude_pathways = c("1-1-3","1-3-1"),pathway_length = 8)
#' test_itemcat_con(x = test_mstATA,
#'                              attribute ="itemtype",cat_levels = c("MC","TEI"),
#'                              operator = "=",
#'                              target_num = c(7,1),
#'                              which_pathway = 1:7)
#'
#' # Example 3: constraints on combinations of categorical attributes
#' mini_itempool$content_itemtype <-
#'   paste(mini_itempool$content, mini_itempool$itemtype)
#'
#' test_mstATA <- mst_design(
#'   itempool = mini_itempool,
#'   design = "1-3-3",
#'   exclude_pathways = c("1-1-3","1-3-1"),
#'   pathway_length = 8
#' )
#' test_itemcat_con(
#'   x = test_mstATA,
#'   attribute = "content_itemtype",
#'   cat_levels = paste0("content", rep(1:4, each = 2)," ", c("MC","TEI")),
#'   operator = "=",
#'   target_num = c(1,1, 1,1, 1,0, 2,1),
#'   which_pathway = 1:7
#' )
#' @export


test_itemcat_con<-function(x,attribute,cat_levels,operator,target_num,
                           which_module=NULL,which_pathway=NULL){
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
  NumStages <- x$NumStages
  NumModules <- x$NumModules
  NumPathways<-x$NumPathways
  PathwayIndex<-x$PathwayIndex
  num_decisions<-PoolSize*NumModules

  check_scope<-check_test_constraint_scope(num_modules = NumModules,num_pathways = NumPathways,
                                           which_module = which_module, which_pathway = which_pathway)
  application_level<-check_scope[["application_level"]]
  which_module <- check_scope[["which_module"]]
  which_pathway <- check_scope[["which_pathway"]]
  total_rows<-check_scope[["total_rows"]]
  row_ids<-check_scope[["row_ids"]]
  target_num<-expand_target_to_matrix(input = target_num,levels = cat_levels,total_rows = total_rows,
                                      row_ids = row_ids)
  check_nonneg_integer(input = target_num,input_name = "target_num")

  category_vec<-check_attribute_column(itempool = ItemPool,attribute = attribute)
  NumCategory <- length(cat_levels)
  CategoryIndex <- vapply(seq_len(NumCategory),FUN = function(level_id) {
    get_attribute_val(ItemPool,attribute = attribute,cat_level = cat_levels[level_id]) },FUN.VALUE = integer(PoolSize))

  i_idx<-integer()
  j_idx<-integer()
  ConstraintMatrix_name<-c()
  if(application_level=="Module-level"){
    n_modules<-length(which_module)
    num_constraints<-n_modules*NumCategory
    rhs<-numeric()
    col_offset<-(which_module-1)*PoolSize
    for(category_id in 1:NumCategory){
      cate_start<-1+n_modules*(category_id-1L)
      cate_end<-n_modules*category_id
      item_idx<-which(CategoryIndex[,category_id]==1)
      i_idx<-c(i_idx,rep(c(cate_start:cate_end),each = length(item_idx)))
      j_idx<-c(j_idx,item_idx+rep(col_offset,each=length(item_idx)))
      rhs<-c(rhs,target_num[,category_id])
    }
    ConstraintMatrix_name<-c(ConstraintMatrix_name,
                             paste("The",name,"number of items from",rep(cat_levels,each = n_modules),"in Module",which_module))
    Specification<-data.frame(a=paste0("Item count from ",paste(cat_levels,collapse = "/")),
                              b=attribute,c="Categorical",d="Module-level",e=name,f=n_modules*NumCategory,
                              stringsAsFactors = FALSE)
  }else {
    n_pathways<-length(which_pathway)
    num_constraints<-n_pathways*NumCategory
    rhs<-numeric()
    modules_involved<-vapply(which_pathway,FUN = function(pathway_id) {
      as.vector(as.matrix(PathwayIndex[PathwayIndex$pathway_index==pathway_id,1:NumStages]))
    },FUN.VALUE = integer(NumStages))
    col_offset<-apply(modules_involved,MARGIN = 2,FUN = function(module_id) (module_id-1L)*PoolSize)
    col_offset_full<-as.vector(col_offset)
    for(category_id in 1:NumCategory){
      cate_start<-1+n_pathways*(category_id-1L)
      cate_end<-n_pathways*category_id
      item_idx<-which(CategoryIndex[,category_id]==1)
      i_idx<-c(i_idx,rep(c(cate_start:cate_end),each = NumStages*length(item_idx)))
      j_idx <- c(j_idx, item_idx+rep(col_offset_full,each=length(item_idx)))
      rhs <- c(rhs,target_num[, category_id])
    }
    ConstraintMatrix_name<-c(ConstraintMatrix_name,
                             paste("The",name,"number of items from",rep(cat_levels,each=n_pathways),"in Pathway",which_pathway))
    Specification<-data.frame(a=paste0("Item count from ",paste(cat_levels,collapse = "/")),
                              b=attribute,c="Categorical",d="Pathway-level",e=name,f=n_pathways*NumCategory,
                              stringsAsFactors = FALSE)
  }
  colnames(Specification)<-c("Requirement","Attribute","Type","Application Level","Operator","Num of Constraints")
  Specification$`Num of Constraints`<-as.numeric(Specification$`Num of Constraints`)
  ConstraintMatrix<-Matrix::sparseMatrix(i = i_idx,j = j_idx,x = 1L,dims = c(num_constraints,num_decisions))
  operators<-rep(operator,nrow(ConstraintMatrix))
  decisionvar_name<-x$decisionvar_name
  colnames(ConstraintMatrix)<-paste0("x[", rep(seq_len(NumModules), each = PoolSize), ",", rep(seq_len(PoolSize), NumModules), "]")
  if(length(decisionvar_name)!=num_decisions){
    ConstraintMatrix<-ConstraintMatrix[,decisionvar_name]
  }
  return(create_constraint(name = ConstraintMatrix_name,
                           specification = Specification,
                           A_binary=ConstraintMatrix,A_real = NULL,
                           operators=operators,d=rhs,
                           C_binary = NULL,C_real = NULL))
}


