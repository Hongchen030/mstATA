#' @title Generate Itemset-Level Constraints for Minimum, Exact, or Maximum Numbers of
#' Stimulus-Linked Items from Specific Categorical Levels
#'
#' @description
#' This function constructs linear constraints ensuring that, for every selected
#' stimulus, the items associated with that stimulus meet the required minimum,
#' exact, or maximum counts from specified categorical levels (e.g., content
#' area, skill type, item format).

#'
#' The total number of generated linear constraints =
#'
#' (number of stimuli) x (number of category levels) x (number of involved modules)
#'
#' For a range constraint (i.e., both a minimum and a maximum number of items
#' requirement), the function must be called twice:
#' \itemize{
#'   \item once with \code{target_num = min} and \code{operator = ">="}, and
#'   \item once with \code{target_num = max} and \code{operator = "<="}.
#' }
#'
#' @param x An object of class \code{"mstATA_design"} created by \code{mst_design()}.
#' @param attribute A string giving the column name in \code{x$ItemPool} that
#'   represents the **item-level categorical attribute**.
#' @param cat_levels Character vector of category levels to constrain. Must match values
#'   in \code{attribute}.
#' @param operator A character string specifying the inequality or equality
#'   operator. Must be one of \code{"<="}, \code{"="}, or \code{">="}.
#' @param target_num A scalar/vector specifying the required number of items for
#' the category levels in \code{cat_levels}.
#' @param which_module Integer vector of modules.
#' Must be consistent with the
#' choices made in:
#' \itemize{
#'   \item \code{test_stimcount_con()}
#'   \item \code{test_stimcat_con()}
#'   \item \code{test_stimquant_con()}
#' }
#' @param which_pathway Integer vector of pathways.
#' Must be consistent with the
#' choices made in:
#' \itemize{
#'   \item \code{test_stimcount_con()}
#'   \item \code{test_stimcat_con()}
#'   \item \code{test_stimquant_con()}
#' }
#'
#' @details
#'
#' The constraint enforced is:
#'
#' \strong{If a stimulus is selected, a minimum, exact, or maximum number of its
#' linked items must belong to the specified category level.}
#'
#' The key properties of this constraint are:
#'
#' \itemize{
#'
#'   \item The attribute type is \emph{logical}: a conditional
#'   "if-then" relationship between the selection of a stimulus and the required
#'   selection of its associated items.
#'
#'   \item The attribute is defined at the \emph{itemset level: set size for each stimulus} in the item pool
#'   through the mapping between a stimulus and the items that belong to it.
#'
#'   \item \emph{Conditional} stimulus-item constraints (such as
#'   \link{stim_itemcount_con}(), \link{stim_itemcat_con}(),
#'   \link{stim_itemquant_con}()) must be applied at the \code{"Module-level"} because
#'   \strong{items linked to a selected stimulus cannot be distributed across multiple modules}: if the stimulus is
#'   selected in a module, all items required from that stimulus must appear in
#'   the same module.
#'
#'   \item \strong{Important:} The arguments \code{which_module} and
#'   \code{which_pathway} must be consistent with the choices made in
#'   \link{test_stimcount_con}(), \link{test_stimcat_con}(),
#'   and \link{test_stimquant_con}().
#' }
#'
#'
#' @section Mathematical Formulation:
#'
#' Suppose the item pool contains (S - 1) stimulus-based item sets, indexed by
#' \eqn{s = 1, \ldots, S - 1}{s = 1, \ldots, S - 1}. Each stimulus has a designated pivot item,
#' indexed by \eqn{i^*_s}{i*s}. In addition, the pool contains a set of discrete
#' (non-stimulus-based) items, which are represented by a dummy stimulus
#' \eqn{s = S}{s = S} to allow a unified indexing scheme. Items belonging to stimulus
#' \eqn{s}{s} are indexed as \eqn{i_s = 1, 2, \ldots, I_s}{is = 1,2,...,Is}.
#'
#' Suppose there are \eqn{M}{M} modules in an MST panel. Let
#' \eqn{m = 1, \ldots, M}{m = 1, \ldots, M} denote the module index.
#'
#' Let \eqn{V_c^{item}}{V_c^{item}} denote the set of
#' items that belong to category c.
#'
#'
#' \deqn{
#'   n_c^{\min}  x_{i_s^{*},m}
#'    \le
#'   \sum_{i_s \in V_c^{item}} x_{i_s,m}
#'    \le
#'   n_c^{\max}  x_{i_s^{*},m}, s = 1, \ldots, S - 1.
#' }
#'
#' Here:
#' \itemize{
#'   \item \eqn{x_{i_s,m}}{x_{i_s,m}} is the binary decision variable indicating whether
#'   item \eqn{i_s}{i_s} is selected into module m.
#'
#'   \item \eqn{x_{i_s^{*},m}}{x_is*m} indicates whether the pivot item for stimulus
#'   s is selected into module m, thereby indicating whether the
#'   stimulus s is selected in that module.
#'
#'   \item \eqn{n_c^{\min}}{n_c^{\min}} and \eqn{n_c^{\max}}{n_c^{\max}} are the required minimum and maximum
#'   number of stimulus-based items from category level c if stimulus s is selected.
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
#' @seealso
#'
#' [stim_itemcount_con()],
#'
#' [stim_itemquant_con()]
#' @examples
#' data("reading_itempool")
#'
#' pivot_stim_map <- create_pivot_stimulus_map(
#'   itempool   = reading_itempool,
#'   stimulus   = "stimulus",
#'   pivot_item = "pivot_item"
#' )
#'
#' test_mstATA <- mst_design(
#'   itempool      = reading_itempool,
#'   design        = "1-3-3",
#'   module_length = c(14,12,12,12,12,12,12),
#'   pivot_stim_map = pivot_stim_map
#' )
#'
#' # Example 1: At least 2 MC and at least 1 TEI item must be selected when the stimulus is selected.
#' stim_itemcat_con(
#'   x = test_mstATA,
#'   attribute = "itemtype",
#'   cat_levels = c("MC","TEI"),
#'   operator = ">=",
#'   target_num = c(2,1)
#' )
#'
#' # Example 2: At most 5 MC and at most 3 TEI items may be selected from each stimulus.
#' stim_itemcat_con(
#'   x = test_mstATA,
#'   attribute = "itemtype",
#'   cat_levels = c("MC","TEI"),
#'   operator = "<=",
#'   target_num = c(5,3)
#' )
#'
#' # Example 3: Exactly 4 MC and 2 TEI items must be selected from each stimulus.
#' stim_itemcat_con(
#'   x = test_mstATA,
#'   attribute = "itemtype",
#'   cat_levels = c("MC","TEI"),
#'   operator = "=",
#'   target_num = c(4,2)
#' )
#' @export


stim_itemcat_con<-function(x,attribute,cat_levels,
                           operator, target_num,
                           which_module = NULL,which_pathway = NULL){

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
  pivot_item_ids     <- pivot_stim_map$pivot_item_id
  stimulus_name <- pivot_stim_map$stimulus_name
  item_ids<-pivot_stim_map$stimulus_members
  NumStimulus<-length(pivot_item_ids)

  ItemPool <- x$ItemPool
  PoolSize <- nrow(ItemPool)
  NumStages <- x$NumStages
  NumModules <- x$NumModules
  NumPathways <- x$NumPathways
  PathwayIndex<-x$PathwayIndex
  num_decisions<-PoolSize*NumModules

  check_scope<-check_test_constraint_scope(num_modules = NumModules,num_pathways = NumPathways,
                                           which_module = which_module, which_pathway = which_pathway)
  application_level<-check_scope[["application_level"]]
  which_module <- check_scope[["which_module"]]
  which_pathway <- check_scope[["which_pathway"]]
  if(application_level=="Module-level"){
    which_module<-which_module
  }else{
    which_module<-sort(unique(unlist(as.vector(PathwayIndex[PathwayIndex$pathway_index %in% which_pathway,1:NumStages]))))
  }
  n_modules<-length(which_module)

  operator<-check_operator(operator = operator)
  if(operator==">="){
    name<-"(min)"
  }else if(operator=="<="){
    name<-"(max)"
  }else{
    name<-"(exact)"
  }

  category_vec<-check_attribute_column(itempool = ItemPool,attribute  = attribute)
  NumCategory<- length(cat_levels)

  check_nonneg_integer(input = target_num,input_name = "target_num")
  if(length(target_num) == 1){
    target_num<-rep(target_num,NumCategory)
  }
  if(length(target_num)!=NumCategory){
    stop("The length of 'target_num' must be the same as the length of 'cat_levels'")
  }

  i_idx<-integer()
  j_idx<-integer()
  x_value<-integer()
  operators<-character()
  ConstraintMatrix_name<-character()
  rhs<-numeric()
  num_constraints<-NumCategory*NumStimulus*n_modules
  col_offsets<-PoolSize*(which_module-1L)

  for(category_id in 1:NumCategory){
    cat_start<-1L+n_modules*NumStimulus*(category_id-1L)
    cat_target<-target_num[category_id]
    for(stim_id in 1:NumStimulus){
      pivot_item<-pivot_item_ids[stim_id]
      items<-item_ids[[stim_id]]
      stimcat_start<-cat_start+n_modules*(stim_id-1L)
      stimcat_rows<-stimcat_start:(stimcat_start+n_modules-1L)
      belong_tocat<-items[which(category_vec[items]==cat_levels[category_id])]
      numIncat<-length(belong_tocat)
      ConstraintMatrix_name<-c(ConstraintMatrix_name,
                               paste("Stimulus", stimulus_name[stim_id],
                                     "module",which_module,
                                     "-",name,cat_target,
                                     "items in category", cat_levels[category_id]))
      if(numIncat==0){
        i_idx<-c(i_idx,stimcat_rows)
        j_idx<-c(j_idx,pivot_item+col_offsets)
        x_value<-c(x_value,rep(-cat_target,n_modules))
      }else if(pivot_item%in%belong_tocat){
        i_idx<-c(i_idx,rep(stimcat_rows,each=numIncat))
        j_items<-rep(belong_tocat,n_modules)
        j_coloffset<-rep(col_offsets,each=numIncat)
        j_idx<-c(j_idx,j_items+j_coloffset)
        temp_x<-rep(1L,numIncat)
        temp_x[which(belong_tocat==pivot_item)]<-(1-cat_target)
        x_value<-c(x_value,rep(temp_x,n_modules))
      }else{
        i_idx<-c(i_idx,rep(stimcat_rows,each=(numIncat+1)))
        j_items<-rep(c(belong_tocat,pivot_item),n_modules)
        j_coloffset<-rep(col_offsets,each=(numIncat+1))
        j_idx<-c(j_idx,j_items+j_coloffset)
        temp_x<-c(rep(1L,numIncat),-cat_target)
        x_value<-c(x_value,rep(temp_x,n_modules))
      }
    }
  }

  ConstraintMatrix <- Matrix::sparseMatrix(i = i_idx,j = j_idx,x = x_value,
                                           dims = c(num_constraints, num_decisions))
  Specification <- data.frame(a = paste0("Within-stimulus item count from ",paste(cat_levels,collapse = "/")),
                              b = attribute,
                              c = "Logical",d = "Module-level",
                              e = name,f = num_constraints,
                              stringsAsFactors = FALSE)
  colnames(Specification)<-c("Requirement","Attribute","Type","Application Level","Operator","Num of Constraints")

  decisionvar_name<-x$decisionvar_name
  colnames(ConstraintMatrix)<-paste0("x[", rep(seq_len(NumModules), each = PoolSize), ",", rep(seq_len(PoolSize), NumModules), "]")
  if(length(decisionvar_name)!=num_decisions){
    ConstraintMatrix<-ConstraintMatrix[,decisionvar_name,drop = FALSE]
  }

  return(create_constraint(name = ConstraintMatrix_name,
                           specification = Specification,
                           A_binary=ConstraintMatrix,A_real = NULL,
                           operators=rep(operator,num_constraints),d=rep(0,num_constraints),
                           C_binary = NULL,C_real = NULL))
}
