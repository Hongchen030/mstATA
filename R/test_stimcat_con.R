#' @title Generate Module/Pathway-Level Constraints for the Min/Exact/Max Number of Stimuli
#' from Specific Categorical Levels
#'
#' @description
#' This function generates linear constraints on the number of *stimuli* belonging
#' to specified categorical levels (e.g., passage type, theme, genre) in a multistage
#' test (MST).
#'
#' Constraints may be applied at:
#' \itemize{
#'   \item \strong{"Module-level"} — each module is treated as an independent form.
#'   \item \strong{"Pathway-level"} — each pathway is treated as an independent form.
#' }
#'
#' The application level is determined by which of
#' \code{which_module} or \code{which_pathway} is supplied.
#'
#' The total number of constraints depends on the application level and on the
#' number of categorical levels included in \code{cat_levels}:
#'
#'
#' \itemize{
#'   \item \strong{Module-level:} when \code{which_module} is provided
#'          OR both \code{which_module} and \code{which_pathway} are \code{NULL}
#'
#'    The number of constraints is
#'     \strong{(number of category levels) × (number of modules specified)}
#'
#'   \item \strong{Pathway-level:} when \code{which_pathway} is provided and
#'         \code{which_module = NULL}
#'
#'    The number of constraints is
#'     \strong{(number of category levels) × (number of pathways specified)}
#' }
#'
#' Users may constrain only a subset of modules or pathways using
#' \code{which_module} or \code{which_pathway}, and may restrict the constraints
#' to a subset of stimulus categories using \code{cat_levels}.
#'
#'
#' @param x An object of class `"mstATA_design"` created by `mst_design()`.
#' @param attribute A string giving the column name in \code{x$ItemPool} that
#'   represents the **stimulus-level categorical attribute**.
#' @param cat_levels Character string or vector of category levels to constrain.
#' @param operator A character string indicating the type of constraint.
#'   Must be one of \code{"<="}, \code{"="}, or \code{">="}.
#' @param target_num Target number of stimuli per category per module/pathway.
#'   Can be a scalar, vector, or matrix (see Details).
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
#' \strong{The test must meet a minimum, exact, or maximum number of stimuli
#' belonging to each specified categorical level.}
#'
#' Key characteristics:
#' \itemize{
#'   \item The attribute type is \emph{categorical}.
#'   \item The attribute is defined at the \emph{stimulus level} in the item pool.
#'   \item The constraints are applied at either \strong{"Module-level"} or \strong{"Pathway-level"}.
#' }
#'
#' **2. Pivot-Item Method**
#'
#' MST assembly uses the \strong{pivot-item formulation} (van der Linden, 2005):
#'
#' \itemize{
#'   \item Each stimulus has one designated pivot item \eqn{i_s^{*}}.
#'   \item A stimulus is considered selected if and only if its pivot item
#'     is selected.
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
#' Let \eqn{V_c^{stim}} denote the set of
#' stimuli that belong to category c.
#'
#' **1. Module-level constraint (for module m)**
#'
#' \deqn{\sum_{s \in V_c^{stim}} x_{i_s^{*},m} \;\substack{\le \\ \ge \\ =}\; n_c^{stim,m}}
#'
#' **2. Pathway-level constraint (for pathway r)**
#'
#' \deqn{\sum_{m \in r} \sum_{s \in V_c^{stim}} x_{i_s^{*},m} \;\substack{\le \\ \ge \\ =}\; n_c^{stim,r}}
#'
#' Here:
#' \itemize{
#'   \item \eqn{x_{i_s^{*},m}} indicates whether the pivot item for stimulus
#'   s is selected into module m, thereby indicating whether the
#'   stimulus is selected in that module.
#'
#'   \item \eqn{m \in r} denote modules belonging to pathway r.
#'
#'   \item The stacked operator, \eqn{\substack{\leq \\ \geq \\ =}} denotes that the specification may take the form of an upper bound, lower bound, or exact value.
#'
#'   \item \eqn{n_c^{stim,m}} specify the required number of stimuli from category level c in module m.
#'
#'   \item \eqn{n_c^{stim,r}} specify the required number of stimuli from category level c in pathway r.
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
#' # Example 1:
#' # MST 1-3-3 design, 7 modules, 2 stimulus categories (history, social studies).
#' # Stage 1: less than 1 history passage.
#' # Stage 2 & 3: less than 1 history and less than 1 social studies passage in each module.
#'
#' test_mstATA <- mst_design(
#'   itempool = reading_itempool,
#'   design   = "1-3-3",
#'   module_length = c(14,12,12,12,12,12,12),
#'   pivot_stim_map = pivot_stim_map
#' )
#'
#' test_stimcat_con(
#'   x = test_mstATA,
#'   attribute = "stimulus_type",
#'   cat_levels = c("history","social studies"),
#'   operator = "<=",
#'   target_num = matrix(
#'     c(1,0,
#'       rep(c(1,1), 3),
#'       rep(c(1,1), 3)),
#'     nrow = 7, ncol = 2, byrow = TRUE
#'   )
#' )
#'
#'
#' # Example 2:
#' # Pathway REE requires between 1 and 3 history passages.
#' con1<-test_stimcat_con(
#'     x = test_mstATA,
#'     attribute = "stimulus_type",
#'     cat_levels = "history",
#'     operator = ">=",
#'     target_num = 1,
#'     which_pathway = 1
#'   )
#' con2<-test_stimcat_con(
#'     x = test_mstATA,
#'     attribute = "stimulus_type",
#'     cat_levels = "history",
#'     operator = "<=",
#'     target_num = 3,
#'     which_pathway = 1
#'   )
#'
#' @export


test_stimcat_con <- function(x,attribute,cat_levels,operator,target_num,
                             which_module=NULL,which_pathway=NULL){

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
  NumStages <- x$NumStages
  NumModules <- x$NumModules
  NumPathways<-x$NumPathways
  PathwayIndex<-x$PathwayIndex
  num_decisions<-PoolSize*NumModules
  NumCategory <- length(cat_levels)

  pivot_items<-pivot_stim_map$pivot_item_id
  CategoryIndex <- vapply(seq_len(NumCategory),FUN = function(level_id) {
    get_attribute_val(ItemPool[pivot_items,],attribute = attribute,cat_level = cat_levels[level_id]) },FUN.VALUE = integer(length(pivot_items)))
  check_scope<-check_test_constraint_scope(num_modules = NumModules,num_pathways = NumPathways,
                                           which_module = which_module, which_pathway = which_pathway)
  application_level<-check_scope[["application_level"]]
  which_module <- check_scope[["which_module"]]
  which_pathway <- check_scope[["which_pathway"]]
  total_rows<-check_scope[["total_rows"]]
  row_ids<-check_scope[["row_ids"]]
  target_num<-expand_target_to_matrix(input = target_num,levels = cat_levels,
                                      total_rows = total_rows,row_ids = row_ids)
  check_nonneg_integer(target_num,"target_num")
  if (application_level == "Module-level") {
    num_constraints<-length(which_module) * NumCategory
  }else {
    num_constraints<-length(which_pathway) * NumCategory
  }

  i_idx<-integer()
  j_idx<-integer()
  rhs<-numeric(num_constraints)
  ConstraintMatrix_name<-character(num_constraints)
  if(application_level=="Module-level"){
    n_modules<-length(which_module)
    col_offset<-(which_module-1L)*PoolSize
    for(category_id in 1:NumCategory){
      cate_start<-1+n_modules*(category_id-1L)
      cate_end<-n_modules*category_id
      stim_idx<-pivot_items[which(CategoryIndex[,category_id]==1)]
      i_idx<-c(i_idx,rep(cate_start:cate_end,each=length(stim_idx)))
      j_idx<-c(j_idx, rep(stim_idx,n_modules)+rep(col_offset,each=length(stim_idx)))
      rhs[cate_start:cate_end]<-target_num[,category_id]
      ConstraintMatrix_name[cate_start:cate_end]<-paste("The",name,"number of stimuli from",
                                                        cat_levels[category_id],"in Module",which_module)
    }

    Specification<-data.frame(a=paste0("Stimulus count from ",paste(cat_levels,collapse = "/")),
                              b=attribute,c="Categorical",d="Module-level",e=name,f=num_constraints,
                              stringsAsFactors = FALSE)

  }else {
    n_pathways<-length(which_pathway)
    modules_involved<-vapply(which_pathway,FUN = function(pathway_id) {
      as.vector(as.matrix(PathwayIndex[PathwayIndex$pathway_index==pathway_id,1:NumStages]))
    },FUN.VALUE = integer(NumStages))
    modules_involved_vec<-as.vector(modules_involved)
    col_offset<-PoolSize*(modules_involved_vec-1L)
    for(category_id in 1:NumCategory){
      cate_start<-1+n_pathways*(category_id-1)
      cate_end<-n_pathways*category_id
      stim_idx<-pivot_items[which(CategoryIndex[,category_id]==1)]
      i_idx<-c(i_idx,rep(cate_start:cate_end,each=length(stim_idx)*NumStages))
      j_idx<-c(j_idx,rep(stim_idx,NumStages*n_pathways)+rep(col_offset,each=length(stim_idx)))
      rhs[cate_start:cate_end]<-target_num[,category_id]
      ConstraintMatrix_name[cate_start:cate_end]<-paste("The",name,"number of stimuli from",
                                                        cat_levels[category_id],"in Pathway",which_pathway)
    }
    Specification<-data.frame(a=paste0("Stimulus count from ",paste(cat_levels,collapse = "/")),
                              b=attribute,c="Categorical",d="Pathway-level",e=name,f=num_constraints,
                              stringsAsFactors = FALSE)
  }
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

