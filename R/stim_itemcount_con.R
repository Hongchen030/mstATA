#' @title Generate Itemset-Level Constraints for the Number of Selected Items
#'
#' @description
#' This function constructs linear constraints that enforce minimum, exact, or
#' maximum counts on the number of items associated with a selected stimulus.
#' It supports both full selection (all items linked to a stimulus must be
#' selected if the stimulus is selected) and partial selection (only a subset
#' of linked items must be selected).
#'
#' The total number of generated linear constraints depends on the provided values of
#' \code{min} and \code{max}.
#'
#' \itemize{
#'
#'   \item \strong{Full selection (min = NULL and max = NULL):}
#'     All items linked to a stimulus must either all be selected or all not be
#'     selected.
#'
#'  The number of constraints is
#'     \strong{(number of stimuli) × (number of invovled modules)}.
#'
#'   \item \strong{Only max is specified:}
#'     An upper bound is imposed, but no lower bound.
#'
#'  The number of constraints is
#'     \strong{(number of stimuli) × (number of invovled modules)}.
#'
#'   \item \strong{Only min is specified:}
#'     A lower bound is imposed; the upper bound defaults to the total number of
#'     items belonging to each stimulus.
#'
#'  The number of constraints is
#'     \strong{2 × (number of stimuli) × (number of invovled modules)}.
#'
#'   \item \strong{Both min and max are specified:}
#'
#'  If min ≠ max (lower and upper bound are both active), the number of constraints is
#'     \strong{2 × (number of stimuli) × (number of invovled modules)}.
#'
#'  If min = max (exact number), the number of constraints is
#'     \strong{(number of stimuli) × (number of invovled modules)}.
#'
#' }
#'
#'
#' @param x An object of class `"mstATA_design"` created by `mst_design()`.
#' @param min An optional scalar specifying the minimum number of items to be
#' included conditional on the selection of its stimulus.
#' @param max An optional scalar specifying the maximum number of items to be
#' included conditional on the selection of its stimulus.
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
#' \strong{If a stimulus is selected, then a minimum, exact, or maximum number
#' of items linked to that stimulus must also be selected.}
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
#' When only \code{min} is provided, the maximum bound \eqn{n^{\max}} is
#' automatically set to the total number of items belonging to stimulus s.
#' This produces a safe gating constraint: if the pivot item is not selected
#' (meaning the stimulus is not selected), then \eqn{x_{i_s^{*},m} = 0} forces
#' all item-selection variables \eqn{x_{m,i_s} = 0}, ensuring that no
#' items from an unselected stimulus can be inadvertently included.
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
#' \deqn{
#'   n^{\min} \, x_{i_s^{*},m}
#'   \;\le\;
#'   \sum_{i_s = 1}^{I_s} x_{i_s,m}
#'   \;\le\;
#'   n^{\max} \, x_{i_s^{*},m},
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
#'   stimulus is selected in that module.
#'
#'   \item \eqn{n^{\min}} and \eqn{n^{\max}} specify the minimum and maximum
#'   allowable number of items that must be selected if its stimulus is selected.
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
#' pivot_stim_map <- create_pivot_stimulus_map(
#'   itempool   = reading_itempool,
#'   stimulus   = "stimulus",
#'   pivot_item = "pivot_item"
#' )
#'
#' test_mstATA <- mst_design(
#'   itempool      = reading_itempool,
#'   design        = "1-3-3",
#'   module_length = c(14, 12, 12, 12, 12, 12, 12),
#'   pivot_stim_map = pivot_stim_map
#' )
#'
#' # Example 1: Full selection.
#' # If a stimulus is selected, all items linked to that stimulus must also be selected.
#' # 7 modules × 10 stimuli = 70 constraints
#' stim_itemcount_con(x = test_mstATA)
#'
#' # Example 2: Range selection (min and max).
#' # If a stimulus is selected, between 2 and 5 of its linked items must be selected.
#' # Lower and upper bounds active → 2 × (7 modules × 10 stimuli) = 140 constraints
#' stim_itemcount_con(
#'   x = test_mstATA,
#'   min = 2,
#'   max = 5
#' )
#'
#' # Example 3: Minimum-only selection.
#' # If a stimulus is selected, at least 5 of its items must be selected.
#' # Upper bound defaults to total items per stimulus → 2 × (7 × 10) = 140 constraints
#' stim_itemcount_con(
#'   x = test_mstATA,
#'   min = 5
#' )
#'
#' # Example 4: Maximum-only selection.
#' # If a stimulus is selected, at most 8 of its items may be selected.
#' # Only upper bound active → 7 modules × 10 stimuli = 70 constraints
#' stim_itemcount_con(
#'   x = test_mstATA,
#'   max = 8
#' )
#'
#' # Example 5: Exact selection.
#' # If a stimulus is selected, exactly 6 of its items must be selected (min = max = 6).
#' # Behaves like a single equality bound → 7 × 10 = 70 constraints
#' stim_itemcount_con(
#'   x = test_mstATA,
#'   min = 6,
#'   max = 6
#' )
#' @seealso [stim_itemcat_con()],[stim_itemquant_con()]
#' @export

stim_itemcount_con <- function(x,
                               min = NULL,max = NULL,
                               which_module = NULL, which_pathway = NULL) {
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
  item_in_stimulus <- pivot_stim_map$numItems_stimulus
  item_ids<-pivot_stim_map$stimulus_members
  NumStimulus<-length(pivot_item_ids)

  ItemPool<-x$ItemPool
  PoolSize <- nrow(ItemPool)
  NumStages <- x$NumStages
  NumModules <- x$NumModules
  NumPathways<-x$NumPathways
  PathwayIndex <- x$PathwayIndex
  num_decisions<-PoolSize*NumModules

  check_scope<-check_test_constraint_scope(num_modules = NumModules,num_pathways = NumPathways,
                                           which_module = which_module, which_pathway = which_pathway)
  application_level<-check_scope[["application_level"]]
  which_module <- check_scope[["which_module"]]
  which_pathway <- check_scope[["which_pathway"]]
  if(application_level=="Module-level"){
    which_module<-which_module
  }else{
    which_module<-sort(unique(unlist(as.vector(PathwayIndex[PathwayIndex$pathway_index %in% which_pathway,
                                                            1:NumStages]))))
  }

  if(is.null(min) && is.null(max)){
    name<-"(exact)"
    side<-1L
  }else{
    validate_min_max(min = min,max = max)
    if(!is.null(min) && !is.null(max)){
      if(min==max){
        name<-"(exact)"
        side<-1L
      }else{
        name <- "(range)"
        side <- 2L
      }
    }
    if(!is.null(min) && is.null(max)){
      name<-"(range)"
      side<-2L
    }
    if(is.null(min) && !is.null(max)){
      name<-"(max)"
      side<-1L
    }
  }

  n_modules<-length(which_module)
  num_constraints<-NumStimulus*n_modules*side
  col_offsets<-PoolSize*(which_module-1L)
  i_idx<-integer()
  j_idx<-integer()
  x_value<-integer()
  ConstraintMatrix_name<-character()
  if(name=="(exact)"){
    rhs<-rep(0,num_constraints)
    operators<-rep("=",num_constraints)
    for(stim_id in seq_len(NumStimulus)){
      items<-item_ids[[stim_id]]
      num_items<-length(items)
      pivot_item<-pivot_item_ids[stim_id]
      if(!is.null(min) && !is.null(max) &&min == max){
        target<-min
        ConstraintMatrix_name<-c(ConstraintMatrix_name,
                                 paste("If",stimulus_name[stim_id],"is selected in module",which_module,
                                       "exact",min,"items are selected from the stimulus"))
      }else{
        target<-num_items
        ConstraintMatrix_name<-c(ConstraintMatrix_name,
                                 paste("All-in:select item",paste(items,collapse = ","),
                                       "if",stimulus_name[stim_id],"is selected in module",which_module))
      }
      temp_x<-rep(1L,num_items)
      temp_x[which(items==pivot_item)]<-(1L-target)
      stim_start<-1L+n_modules*(stim_id-1L)
      stim_end<-n_modules*stim_id
      i_idx<-c(i_idx,rep(stim_start:stim_end,each=num_items))
      j_items<-rep(items,n_modules)
      j_coloffsets<-rep(col_offsets,each=num_items)
      j_idx<-c(j_idx,j_items+j_coloffsets)
      x_value<-c(x_value,rep(temp_x,n_modules))
    }
  }else{
    rhs<-rep(0,num_constraints)
    operators<-rep("<=",num_constraints)
    for(stim_id in 1:NumStimulus){
      items<-item_ids[[stim_id]]
      num_items<-length(items)
      pivot_item<-pivot_item_ids[stim_id]
      j_items<-rep(items,n_modules)
      j_coloffsets<-rep(col_offsets,each=num_items)
      j_temp<-j_items+j_coloffsets
      if(!is.null(max) && is.null(min)){
        stim_start<-1+n_modules*(stim_id-1L)
        stim_end<-n_modules*stim_id
        rows_U<-stim_start:stim_end
        i_idx<-c(i_idx, rep(rows_U,each=num_items))
        j_idx<-c(j_idx,j_temp)
        U_temp_x <- rep(1L, num_items)
        U_temp_x[items == pivot_item] <- 1L -max
        U_temp_x<-rep(U_temp_x,n_modules)
        x_value<-c(x_value,U_temp_x)
        ConstraintMatrix_name<-c(ConstraintMatrix_name,
                                 paste("If",stimulus_name[stim_id],"is selected in module",
                                       which_module,"at most",max,"items are selected from the stimulus"))
      }else{
        stim_start<-1+2L*n_modules*(stim_id-1L)
        stim_end<-2L*n_modules*stim_id
        stim_rows<-stim_start:stim_end
        rows_L<-stim_rows[1:n_modules]
        rows_U<-stim_rows[(1+n_modules):(2L*n_modules)]
        L_temp_x <- rep(-1L, num_items)
        L_temp_x[items == pivot_item] <- min - 1L
        L_temp_x<-rep(L_temp_x,n_modules)
        if(is.null(max)){
          max_s<-num_items
        }else{
          max_s<-max
        }
        U_temp_x <- rep(1L, num_items)
        U_temp_x[items == pivot_item] <- 1L - max_s
        U_temp_x<-rep(U_temp_x,n_modules)
        i_L<-rep(rows_L,each=num_items)
        i_U<-rep(rows_U,each=num_items)
        i_idx<-c(i_idx,i_L,i_U)
        j_idx<-c(j_idx,rep(j_temp,2))
        x_value<-c(x_value,L_temp_x,U_temp_x)
        ConstraintMatrix_name<-c(ConstraintMatrix_name,
                                 paste("If",stimulus_name[stim_id],"is selected in module",
                                       which_module,"at least",min,"items are selected from the stimulus"),
                                 paste("If",stimulus_name[stim_id],"is selected in module",
                                       which_module,"at most",max_s,"items are selected from the stimulus"))
      }
    }
  }

  Specification<-data.frame(a="Within-stimulus item count",
                            b="Stimulus item membership",c="Logical",d="Module-level",
                            e = name,f = num_constraints)
  colnames(Specification)<-c("Requirement","Attribute","Type","Application Level","Operator","Num of Constraints")
  Specification$`Num of Constraints`<-as.numeric(Specification$`Num of Constraints`)

  ConstraintMatrix<-Matrix::sparseMatrix(i = i_idx,j = j_idx,
                                         x = x_value,dims = c(num_constraints,num_decisions))
  colnames(ConstraintMatrix)<-paste0("x[", rep(seq_len(NumModules), each = PoolSize), ",", rep(seq_len(PoolSize), NumModules), "]")
  decisionvar_name<-x$decisionvar_name
  if(length(decisionvar_name)!=num_decisions){
    ConstraintMatrix<-ConstraintMatrix[,decisionvar_name,drop=FALSE]
  }

  return(create_constraint(name=ConstraintMatrix_name,
                           specification = Specification,
                           A_binary = ConstraintMatrix,A_real = NULL,
                           operators= operators,d = rhs,
                           C_binary = NULL,C_real = NULL))
}

