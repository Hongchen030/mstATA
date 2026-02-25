#' @title Generate Itemset-Level Constraints for Minimum/Exact/Maximum Numbers of
#' Stimulus-Linked Items from Specific Categorical Levels
#'
#' @description
#' This function constructs linear constraints ensuring that, for every selected
#' stimulus, the items associated with that stimulus meet the required minimum,
#' exact, or maximum counts from specified categorical levels (e.g., content
#' area, skill type, item format).
#'
#' The total number of generated constraints depends on the number of involved modules,
#' the provided values of \code{min} and \code{max} and on the number of category levels included in
#' \code{cat_levels}.
#'
#' \itemize{
#'
#'   \item \strong{Full selection (\code{min = NULL} and \code{max = NULL}):}
#'     If a stimulus is selected, items linked to that stimulus and belonging
#'     to the specified category level must all be selected.
#'
#'   The number of constraints:
#'    \strong{(number of category levels) × (number of stimuli) × (number of invovled modules)}.
#'
#'   \item \strong{Only \code{max} is specified:}
#'     Only upper bounds are imposed per category level.
#'
#'   The number of constraints:
#'    \strong{(number of category levels) × (number of stimuli) × (number of invovled modules)}.
#'
#'   \item \strong{Only \code{min} is specified:}
#'     Lower bounds are imposed; upper bounds default to the total number of
#'     items within each category level belonging to each stimulus.
#'
#'  The number of constraints:
#'    \strong{2 × (number of category levels) × (number of stimuli) × (number of invovled modules)}.
#'
#'   \item \strong{Both \code{min} and \code{max} specified}
#'     Lower and upper bounds are both active.
#'
#'  If min ≠ max (lower and upper bound are both active), the number of constraints:
#'     \strong{2 × (number of category levels) × (number of stimuli) × (number of invovled modules)}.
#'
#'  If min = max (exact number), the number of constraints:
#'    \strong{(number of category levels) × (number of stimuli) × (number of invovled modules)}.
#'
#'
#' }
#'
#' @param x An object of class \code{"mstATA_design"} created by \code{mst_design()}.
#' @param attribute A string giving the column name in \code{x$ItemPool} that
#'   represents the **item-level categorical attribute**.
#' @param cat_levels Character vector of category levels to constrain. Must match values
#'   in \code{attribute}.
#' @param min Optional lower bound (scalar, vector, or matrix) specifying the
#'   minimum number of items from each specified category level that must be
#'   selected when the stimulus is selected.
#' @param max Optional upper bound (scalar, vector, or matrix) specifying the
#'   maximum number of items from each specified category level that may be
#'   selected when the stimulus is selected.
#' @param which_module Integer vector of modules. Must be consistent with the choices made in
#'   \code{test_stimcount_con()}, \code{test_stimcat_con()},
#'   and \code{test_stimquant_con()}.
#' @param which_pathway Integer vector of pathways. Must be consistent with the choices made in
#'   \code{test_stimcount_con()}, \code{test_stimcat_con()},
#'   and \code{test_stimquant_con()}.
#'
#' @details
#' **1. Specification**
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
#' **2. Why gating is not required**
#'
#' When only \code{min} is provided, the upper bound \eqn{n_c^{\max}} is
#' automatically set to the total number of items in category c within
#' stimulus s.
#'
#' This produces a safe gating constraint: if the pivot item is not selected
#' (meaning the stimulus is not selected), then \eqn{x_{i_s^{*},m} = 0}
#' forces all corresponding item-selection variables \eqn{x_{i_s,m} = 0},
#' ensuring that no items from an unselected stimulus can be selected.
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
#'
#' \deqn{
#'   n_c^{\min} \, x_{i_s^{*},m}
#'   \;\le\;
#'   \sum_{i_s \in V_c^{\mathrm{item}}} x_{i_s,m}
#'   \;\le\;
#'   n_c^{\max} \, x_{i_s^{*},m},
#'   \qquad s = 1, \ldots, S - 1.
#' }
#'
#' Here:
#' \itemize{
#'   \item \eqn{x_{i_s,m}} is the binary decision variable indicating whether
#'   item \eqn{i_s} is selected into module m.
#'
#'   \item \eqn{x_{i_s^{*},m}} indicates whether the pivot item for stimulus
#'   s is selected into module m, thereby indicating whether the
#'   stimulus s is selected in that module.
#'
#'   \item \eqn{n_c^{\min}} and \eqn{n_c^{\max}} are the required minimum and maximum
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
#' @seealso [stim_itemcount_con()],[stim_itemquant_con()]
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
#' # Example 1: Full selection (min = NULL, max = NULL)
#' # If a stimulus is selected, all MC and all TEI items in that stimulus must be selected.
#' # Constraint count:
#' #   7 modules × 10 stimuli × 2 category levels = 140 constraints
#' stim_itemcat_con(
#'   x = test_mstATA,
#'   attribute = "itemtype",
#'   cat_levels = c("MC","TEI")
#' )
#'
#' # Example 2: Range selection (min and max specified)
#' # Between 1–5 MC items and 0–2 TEI items must be selected when the stimulus is selected.
#' # Both lower and upper bounds active → ×2
#' # Constraint count:
#' #   2 × (7 modules × 10 stimuli × 2 category levels) = 280 constraints
#' stim_itemcat_con(
#'   x = test_mstATA,
#'   attribute = "itemtype",
#'   cat_levels = c("MC","TEI"),
#'   min = c(1,0),
#'   max = c(5,2)
#' )
#'
#' # Example 3: Minimum-only
#' # At least 2 MC and at least 1 TEI item must be selected when the stimulus is selected.
#' # Upper bound defaults → both bounds active → ×2
#' # Constraint count:
#' #   2 × (7 modules × 10 stimuli × 2 category levels) = 280 constraints
#' stim_itemcat_con(
#'   x = test_mstATA,
#'   attribute = "itemtype",
#'   cat_levels = c("MC","TEI"),
#'   min = c(2,1)
#' )
#'
#' # Example 4: Maximum-only
#' # At most 5 MC and at most 3 TEI items may be selected from each stimulus.
#' # Only upper bound active
#' # Constraint count:
#' #   7 modules × 10 stimuli × 2 category levels = 140 constraints
#' stim_itemcat_con(
#'   x = test_mstATA,
#'   attribute = "itemtype",
#'   cat_levels = c("MC","TEI"),
#'   max = c(5,3)
#' )
#'
#' # Example 5: Exact selection (min = max)
#' # Exactly 4 MC and 2 TEI items must be selected from each stimulus.
#' # Equality behaves like a single bound → no doubling
#' # Constraint count:
#' #   7 modules × 10 stimuli × 2 category levels = 140 constraints
#' stim_itemcat_con(
#'   x = test_mstATA,
#'   attribute = "itemtype",
#'   cat_levels = c("MC","TEI"),
#'   min = c(4,2),
#'   max = c(4,2)
#' )
#' @export


stim_itemcat_con<-function(x,attribute,cat_levels,
                           min = NULL,max = NULL,
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


  if(is.null(min) && is.null(max)){
    name<-"(exact)"
    side<-1L
  }else{
    if(!is.null(min)){
      exp_min<-expand_target_to_matrix(input = min,levels = cat_levels,
                                       total_rows = NumModules,row_ids = which_module)
      check_nonneg_integer(input = exp_min,input_name = "min")
      if(is.null(max)){
        name<-"(range)"
        side<-2L
      }
    }

    if(!is.null(max)){
      exp_max<-expand_target_to_matrix(input = max,levels = cat_levels,
                                       total_rows = NumModules,row_ids = which_module)
      check_nonneg_integer(input = exp_max,input_name = "max")
      if(is.null(min)){
        name<-"(max)"
        side<-1L
      }
    }

    if(!is.null(min) && !is.null(max)){
      if(any(exp_max-exp_min<0,na.rm = TRUE)){
        stop("'min' cannot be greater than 'max'.")
      }
      if(all(exp_min==exp_max)){
        name<-"(exact)"
        side<-1L
      }else{
        name <- "(range)"
        side <- 2L
      }
    }
  }

  category_vec<-check_attribute_column(itempool = ItemPool,attribute  = attribute)
  NumCategory<- length(cat_levels)
  i_idx<-integer()
  j_idx<-integer()
  x_value<-integer()
  operators<-character()
  ConstraintMatrix_name<-character()
  rhs<-numeric()
  num_constraints<-NumCategory*NumStimulus*n_modules*side
  col_offsets<-PoolSize*(which_module-1L)
  if(name%in%c("(exact)","(max)")){
    rhs<-rep(0,num_constraints)
    for(stim_id in seq_len(NumStimulus)){
      pivot_item<-pivot_item_ids[stim_id]
      items<-item_ids[[stim_id]]
      stim_start<-1+n_modules*NumCategory*(stim_id-1L)
      for(category_id in 1:NumCategory){
        cat_start<-stim_start+n_modules*(category_id-1L)
        stimcat_rows<-cat_start:(cat_start+n_modules-1L)
        belong_tocat<-items[which(category_vec[items]==cat_levels[category_id])]
        numIncat<-length(belong_tocat)
        if(!is.null(min) && !is.null(max)){
          if(all(exp_min==exp_max)){
            target<-as.vector(exp_min[,category_id])
            ConstraintMatrix_name<-c(ConstraintMatrix_name,
                                     paste("Stimulus", stimulus_name[stim_id],
                                           "module",which_module,
                                           "- exact", target,
                                           "items in category", cat_levels[category_id]))
          }
        }

        if(is.null(min) && !is.null(max)){
          target<-as.vector(exp_max[,category_id])
          ConstraintMatrix_name<-c(ConstraintMatrix_name,
                                   paste("Stimulus", stimulus_name[stim_id],
                                         "module",which_module,
                                         "- at most", target,
                                         "items in category", cat_levels[category_id]))
        }

        if(is.null(min) && is.null(max)){
          target<-rep(numIncat,n_modules)
          ConstraintMatrix_name<-c(ConstraintMatrix_name,
                                   paste("Stimulus", stimulus_name[stim_id],
                                         "module",which_module,
                                         "- exact", target,
                                         "items in category", cat_levels[category_id]))
        }

        if(numIncat==0){
          i_idx<-c(i_idx,stimcat_rows)
          j_idx<-c(j_idx,pivot_item+col_offsets)
          x_value<-c(x_value,-target)
        }else if(pivot_item%in%belong_tocat){
          i_idx<-c(i_idx,rep(stimcat_rows,each=numIncat))
          j_items<-rep(belong_tocat,n_modules)
          j_coloffset<-rep(col_offsets,each=numIncat)
          j_idx<-c(j_idx,j_items+j_coloffset)
          for(module in seq_along(which_module)){
            temp_x<-rep(1L,numIncat)
            temp_x[which(belong_tocat==pivot_item)]<-(1-target[module])
            x_value<-c(x_value,temp_x)
          }
        }else{
          i_idx<-c(i_idx,rep(stimcat_rows,each=(numIncat+1)))
          j_items<-rep(c(belong_tocat,pivot_item),n_modules)
          j_coloffset<-rep(col_offsets,each=(numIncat+1))
          j_idx<-c(j_idx,j_items+j_coloffset)
          for(module in seq_along(which_module)){
            temp_x<-c(rep(1L,numIncat),-target[module])
            x_value<-c(x_value,temp_x)
          }
        }
      }
    }

    if(name=="(exact)"){
      operators<-rep("=",num_constraints)
    }else{
      operators<-rep("<=",num_constraints)
    }
  }

  if(name=="(range)"){
    rhs<-rep(0,num_constraints)
    operators<-rep("<=",num_constraints)
    for(stim_id in seq_len(NumStimulus)){
      pivot_item<-pivot_item_ids[stim_id]
      items<-item_ids[[stim_id]]
      stim_start<-1+2L*n_modules*NumCategory*(stim_id-1L)
      for(category_id in 1:NumCategory){
        cat_start<-stim_start+2L*n_modules*(category_id-1L)
        stimcat_rows<-cat_start:(cat_start+2L*n_modules-1L)
        belong_tocat<-items[which(category_vec[items]==cat_levels[category_id])]
        numIncat<-length(belong_tocat)
        if(is.null(max)){
          max_value<-rep(numIncat,n_modules)
        }else{
          max_value<-as.vector(exp_max[,category_id])
        }
        min_value<-as.vector(exp_min[,category_id])
        if(numIncat==0){
          i_idx<-c(i_idx,stimcat_rows)
          j_idx<-c(j_idx,rep((pivot_item+col_offsets),2))
          x_value<-c(x_value,min_value,-max_value)
        }else if(pivot_item%in%belong_tocat){
          i_idx<-c(i_idx,rep(stimcat_rows,each=numIncat))
          j_items<-rep(belong_tocat,2*n_modules)
          j_coloffset<-rep(rep(col_offsets,each=numIncat),2)
          j_idx<-c(j_idx,j_items+j_coloffset)
          for(module in seq_along(which_module)){
            L_temp_x<-rep(-1L,numIncat)
            L_temp_x[which(belong_tocat==pivot_item)]<-min_value[module]-1L
            U_temp_x<-rep(1L,numIncat)
            U_temp_x[which(belong_tocat==pivot_item)]<-1L-max_value[module]
            x_value<-c(x_value,L_temp_x,U_temp_x)
          }
        }else{
          i_idx<-c(i_idx,rep(stimcat_rows,each=(numIncat+1)))
          j_items<-rep(c(belong_tocat,pivot_item),2*n_modules)
          j_coloffset<-rep(rep(col_offsets,each=(numIncat+1)),2)
          j_idx<-c(j_idx,j_items+j_coloffset)
          for(module in seq_along(which_module)){
            L_temp_x<-c(rep(-1L,numIncat),min_value[module])
            U_temp_x<-c(rep(1L,numIncat),-max_value[module])
            x_value<-c(x_value,L_temp_x,U_temp_x)
          }
        }
        ConstraintMatrix_name <- c(ConstraintMatrix_name,
                                   paste("Stimulus", stimulus_name[stim_id],
                                         "module", which_module,
                                         "- at least", min_value,
                                         "items in category", cat_levels[category_id]),
                                   paste("Stimulus", stimulus_name[stim_id],
                                         "module", which_module,
                                         "- at most", max_value,
                                         "items in category", cat_levels[category_id]))
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
    ConstraintMatrix<-ConstraintMatrix[,decisionvar_name]
  }

  return(create_constraint(name = ConstraintMatrix_name,
                           specification = Specification,
                           A_binary=ConstraintMatrix,A_real = NULL,
                           operators=operators,d=rhs,
                           C_binary = NULL,C_real = NULL))
}


