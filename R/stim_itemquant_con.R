#' @title Generate Itemset-Level Constraints for Minimum/Exact/Maximum Sum
#' of Item Quantitative Attribute Values
#'
#' @description
#' Creates a conditional quantitative constraint requiring that, if a stimulus
#' is selected, the *sum of quantitative attribute values* of the
#' items linked to that stimulus must satisfy a user-specified lower bound,
#' upper bound, or exact value.
#'
#' This function enforces a logical “if–then” relationship between stimulus
#' selection and the quantitative properties (difficulty, response time) of its associated items.
#'
#' The total number of generated linear constraints = (number of stimuli) × (number of invovled modules)
#'
#' For a range constraint (i.e., both a minimum and a maximum quantitative
#' requirement), the function must be called twice:
#' \itemize{
#'   \item once with \code{target_value = min} and \code{operator = ">="}, and
#'   \item once with \code{target_value = max} and \code{operator = "<="}.
#' }
#'
#' @param x An object of class `"mstATA_design"` created by `mst_design()`.
#' @param attribute A string giving the column name in \code{x$ItemPool} that
#'   represents the **item-level quantitative attribute**.
#' @param operator A character string of constraint operators, one of
#'   \code{"<="}, \code{"="}, or \code{">="}.
#' @param target_value Target values in specific module/pathway (\code{which_module}, \code{which_pathway}).
#' @param which_module Integer vector of modules. Must be consistent with the choices made in
#'   \code{test_stimcount_con()}, \code{test_stimcat_con()},
#'   and \code{test_stimquant_con()}.
#' @param which_pathway Integer vector of pathways. Must be consistent with the choices made in
#'   \code{test_stimcount_con()}, \code{test_stimcat_con()},
#'   and \code{test_stimquant_con()}.
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
#' @details
#' **1. Specification**
#'
#' The constraint enforced is:
#'
#' \strong{
#' If a stimulus is selected, then the items linked to it must
#' collectively satisfy a minimum, maximum, or exact sum of a quantitative
#' item attribute.
#' }
#'
#' The key properties of this constraint are:
#'
#' \itemize{
#'
#'   \item The attribute type is \emph{logical}: a conditional
#'   "if–then" relationship between the selection of a stimulus and the required
#'   selection of its associated items.
#'
#'   \item The attribute is defined at the \emph{itemset level: set size for each stimulus} in the item pool
#'   through the mapping between a stimulus and the items that belong to it.
#'
#'   \item \emph{Conditional} stimulus–item constraints (such as
#'   \code{stim_itemcount_con()}, \code{stim_itemcat_con()},
#'   \code{stim_itemquant_con()}) must be applied at the \code{"Module-level"} because
#'   \strong{items linked to a selected stimulus cannot be distributed across multiple modules}: if the stimulus is
#'   selected in a module, all items required from that stimulus must appear in
#'   the same module.
#'
#'   \item \strong{Important:} The arguments \code{which_module} and
#'   \code{which_pathway} must be consistent with the choices made in
#'   \code{test_stimcount_con()}, \code{test_stimcat_con()},
#'   and \code{test_stimquant_con()}.
#' }
#'
#' **2. Why gating is required**
#'
#' Because \eqn{q_{i_s}} may be negative or zero, the constraint alone does
#' \emph{not} guarantee that no items are selected when the stimulus is unselected
#' (\eqn{x_{i_s^{*},m} = 0}). Terms with negative coefficients can offset positive
#' terms, making the inequality feasible even when items are improperly selected.
#'
#' Therefore, quantitative stimulus–item constraints must be paired with
#' either \code{stim_itemcount_con()} or \code{stim_itemcat_con()} constraint to guarantee logical consistency.
#'
#'
#' \deqn{x_{i_s^{*},m} = 0 \;\Rightarrow\; x_{i_s,m} = 0,}
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
#' Let \eqn{q_{i_s}} be the quantitative attribute value of item \eqn{i_s}. Then for
#' module m, the conditional constraint is:
#'
#' \deqn{
#' \sum_{i_s=1}^{I_s} q_{i_s} x_{i_s,m}
#' \;\; \substack{\le \\ \ge \\ =} \;\;
#' b_{q}^{m} \; x_{i_s^{*},m},
#' \qquad s = 1, \ldots, S - 1.
#' }
#'
#' Here:
#'
#' \itemize{
#'   \item \eqn{x_{i_s,m}} is the binary decision variable indicating whether
#'   item \eqn{i_s} is selected into module m.
#'
#'   \item \eqn{x_{i_s^{*},m}} indicates whether the pivot item for stimulus
#'   s is selected into module m, thereby indicating whether the
#'   stimulus s is selected in that module.
#'
#'   \item \eqn{b_{q}^{m}} is the required quantitative bound for the sum of
#'   item-level quantitative attribute linked to every stimulus
#'   in module m (minimum, maximum, or exact value).
#' }
#'
#'
#' @examples
#' data("reading_itempool")
#' pivot_stim_map<-create_pivot_stimulus_map(itempool = reading_itempool,
#'                                           stimulus = "stimulus",
#'                                           pivot_item = "pivot_item")
#' # Example 1: if a stimulus is chosen, the sum difficulty of its linked items
#' # may be required to be smaller than 0.5.
#' test_mstATA<-mst_design(itempool = reading_itempool,design = "1-3-3",
#'                          module_length = c(14,12,12,12,12,12,12),
#'                         pivot_stim_map = pivot_stim_map)
#' stim_itemquant_con(x = test_mstATA,attribute = "difficulty",
#'                    operator = "<=",target_value = 0.5,
#'                   which_module = NULL,which_pathway = NULL)
#'
#' @seealso [stim_itemcat_con()],[stim_itemcount_con()]
#' @export


stim_itemquant_con <- function(x, attribute, operator, target_value,
                               which_module = NULL,which_pathway = NULL) {
  if (!inherits(x, "mstATA_design")) {
    stop("Input 'x' must be an object of class 'mstATA_design'.")
  }

  pivot_stim_map<-x$pivot_stim_map
  if (is.null(pivot_stim_map)) {
    stop("Stimulus-based constraints require `pivot_stim_map` ","to be provided in `mst_design()`.",
         call. = FALSE)
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

  operator<-check_operator(operator = operator)
  if(operator==">="){
    name<-"(min)"
  }else if(operator=="<="){
    name<-"(max)"
  }else{
    name<-"(exact)"
  }
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

  target_value<-expand_target_to_matrix(input = target_value,levels = NULL,
                                      total_rows = NumModules,row_ids = which_module)[,1]


  item_vals <- check_attribute_column(itempool = ItemPool,attribute = attribute)
  i_idx <- integer()
  j_idx <- integer()
  x_value<-numeric()
  n_modules<-length(which_module)
  num_constraints <- n_modules*NumStimulus
  col_offsets<-PoolSize*(which_module-1L)
  ConstraintMatrix_name<-character(num_constraints)
  for(stim_id in 1:NumStimulus){
    items<-item_ids[[stim_id]]
    pivot_item<-pivot_item_ids[stim_id]
    stim_start<-1L+n_modules*(stim_id-1L)
    stim_end<-n_modules*stim_id
    stim_rows<-stim_start:stim_end
    i_idx<-c(i_idx,rep(stim_rows,each=length(items)))
    j_items<-rep(items,n_modules)
    j_coloffset<-rep(col_offsets,each=length(items))
    j_idx<-c(j_idx,j_items+j_coloffset)
    for(module in seq_along(which_module)){
      temp_x<-item_vals[items]
      temp_x[which(items==pivot_item)]<-item_vals[pivot_item]-target_value[module]
      x_value<-c(x_value,temp_x)
    }
    ConstraintMatrix_name[stim_rows]<-paste0("Stimulus ", stimulus_name[stim_id],
                                                ", module ", which_module,
                                                ": sum(", attribute, ")",operator," ", target_value)
  }
  Specification <- data.frame(a = paste0("Within-stimulus sum of ", attribute),
                              b = paste("Stimulus item",attribute),
                              c = "Logical",d = "Module-level",
                              e = name,f = num_constraints,
                              stringsAsFactors = FALSE)
  colnames(Specification)<-c("Requirement","Attribute","Type","Application Level","Operator","Num of Constraints")
  ConstraintMatrix<- Matrix::sparseMatrix(i = i_idx, j = j_idx, x = x_value,
                                          dims = c(num_constraints, num_decisions))

  decisionvar_name<-x$decisionvar_name
  colnames(ConstraintMatrix)<-paste0("x[", rep(seq_len(NumModules), each = PoolSize), ",", rep(seq_len(PoolSize), NumModules), "]")
  if(length(decisionvar_name)!=num_decisions){
    ConstraintMatrix<-ConstraintMatrix[,decisionvar_name,drop=FALSE]
  }
  return(create_constraint(name = ConstraintMatrix_name,
                           specification = Specification,
                           A_binary = ConstraintMatrix,A_real = NULL,
                           operators = rep(operator,num_constraints),d = rep(0,num_constraints),
                           C_binary = NULL,C_real = NULL))
}
