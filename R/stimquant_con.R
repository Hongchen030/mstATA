#' @title Generate Stimulus-Level Constraints Requiring Stimulus-Level Quantitative Attributes
#' to Be Greater Than, Less Than, or Within a Specified Range
#'
#' @description
#' This function generates linear constraints ensuring that each selected stimulus
#' satisfies a specified quantitative requirement (e.g., stimulus word count, stimulus
#' difficulty, readability index).
#'
#' This constraint can be enforced at the `"Module-level"`, `"Pathway-level"`, or
#' `"Panel-level"`. It should be mentioned that the formulation requires
#' stimulus-level attributes to be strictly positive.
#'
#' The number of generated linear constraints depends on the application level:
#'
#' \itemize{
#'   \item \strong{Module-level:} when \code{which_module} is provided and
#'         \code{which_pathway = NULL}
#'
#'    The number of constraints is
#'     \strong{(number of stimuli) x (number of modules specified) x side}
#'
#'   \item \strong{Pathway-level:} when \code{which_pathway} is provided and
#'         \code{which_module = NULL}
#'
#'    The number of constraints is
#'     \strong{(number of stimuli) x (number of unique modules in specified pathways) x side}
#'
#'   \item \strong{Panel-level:} both \code{which_module} and \code{which_pathway} are \code{NULL}
#'
#'    The number of constraints is
#'     \strong{(number of stimuli) x (total number of modules) x side}
#' }
#'
#' where \code{side = 1} for one-sided constraints (min-only or max-only)
#' and \code{side = 2} when both \code{min} and \code{max} are provided and
#' \code{min ≠ max}.
#'
#' @param x An object of class `"mstATA_design"` created by `mst_design()`.
#' @param attribute A string giving the column name in `x$ItemPool` that represents the quantitative attribute for the stimulus.
#' @param min A numeric scalar for the lower bound.
#' @param max A numeric scalar for the upper bound.
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
#' \strong{Every selected stimulus must satisfy the required stimulus-level quantitative attribute bound(s).}
#'
#' \itemize{
#'   \item The attribute type is \emph{quantitative}.
#'   \item The attribute is defined at the \emph{stimulus level} in the item pool.
#'   \item Application levels:
#'     \itemize{
#'       \item \strong{Module-level}: enforce stimuli selected in specific module(s) satisfy the quantitative requirement.
#'       \item \strong{Pathway-level}: enforce stimuli selected in any module of a pathway satisfy the quantitative requirement.
#'       \item \strong{Panel-level}: enforce stimuli selected in the panel satisfy the quantitative requirement.
#'     }
#'  }
#'
#'
#' **2. Important Restriction**
#'
#' This function assumes that all stimulus-level quantitative attributes
#' \eqn{q_s} are strictly positive.
#'
#' If stimulus attributes may be zero or negative (e.g., stimulus-level difficulty,
#' centered difficulty values), this constraint formulation does not correctly enforce quantitative
#' bounds. In such cases, users should instead apply stimulus-level selection
#' logic using \code{item_module_eligibility()} inside \code{mst_design()}, which
#' filters eligible stimuli (and linked items) before assembly.
#'
#' For example, to require all routing-module stimuli to have text feature scores
#' between \eqn{-1} and \eqn{1}, users should filter stimuli into the routing
#' module eligibility set using \code{item_module_eligibility()}.
#'
#'
#' @section Mathematical Formulation:
#'
#' Suppose the item pool contains (S - 1) stimulus-based item sets, indexed by
#' \eqn{s = 1, \ldots, S - 1}. Each stimulus has a designated pivot item,
#' indexed by \eqn{i_s^{*}}. In addition, the pool contains a set of discrete
#' (non-stimulus-based) items, which are represented by a dummy stimulus
#' \eqn{s = S} to allow a unified indexing scheme. Items belonging to stimulus
#' \eqn{s} are indexed as \eqn{i_s = 1, 2, \ldots, I_s}.
#'
#' Suppose there are \eqn{M} modules in an MST panel. Let
#' \eqn{m = 1, \ldots, M} denote the module index.
#'
#' Let \eqn{q_s} be the quantitative attribute value of stimulus \eqn{s}.
#'
#' One-sided upper-bound constraint:
#'
#' \deqn{
#'   q_s \, x_{i_s^{*},m} \le b_q^{stim,max},  \qquad s = 1, \ldots, S - 1.
#' }
#'
#' One-sided lower-bound constraint:
#'
#' \deqn{
#'   b_q^{stim,min} \, x_{i_s^{*},m} \le q_s,  \qquad s = 1, \ldots, S - 1.
#' }
#'
#' For two-sided range constraints, both inequalities are included.
#'  (\code{min} and \code{max} both provided and not
#' equal), the number of constraints is doubled (\code{side = 2}).
#' For one-sided constraints, \code{side = 1}.
#'
#' Here:
#' \itemize{
#'  \item \eqn{x_{i_s^{*},m}} indicates whether the pivot item for stimulus
#'   s is selected into module m, thereby indicating whether the
#'   stimulus s is selected in that module.
#'
#'  \item \eqn{q_s} (**must be positive values**) denote the values of the quantitative attribute for stimulus s.
#'
#'  \item \eqn{b_q^{stim,min}} and \eqn{b_q^{stim,max}} are the lower and
#' upper allowable bounds for the attribute \eqn{q_s}.
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
#'   \item{d}{A right-hand-side vector of 1s or 0s depending on whether \code{select} is \code{TRUE} or \code{FALSE}.}
#'   \item{C_binary}{NULL for 'mstATA_constraint' object}
#'   \item{C_real}{NULL for 'mstATA_constraint' object}
#'   \item{sense}{NULL for 'mstATA_constraint' object}
#' }
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
#' # Every selected stimulus in the routing module has 80-110 words.
#' stimquant_con(
#'   x = test_mstATA,
#'   attribute = "stimulus_words",
#'   min = 80, max = 110, which_module =1
#' )
#' @export

stimquant_con<-function(x,attribute,min = NULL,max = NULL,
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
  pivot_item_id     <- pivot_stim_map$pivot_item_id
  stim_names <- pivot_stim_map$stimulus_name
  NumStimulus<-length(pivot_item_id)

  ItemPool<-x$ItemPool
  NumModules<-x$NumModules
  NumPathways<-x$NumPathways
  NumStages<-x$NumStages
  PoolSize<-nrow(ItemPool)
  PathwayIndex<-x$PathwayIndex
  num_decisions<-PoolSize*NumModules

  stimulus_vals <- check_attribute_column(itempool = ItemPool[pivot_item_id,],attribute = attribute)

  if(is.null(min) && is.null(max)){
    stop("At least one of 'min','max' must be provided.")
  }
  validate_min_max(min = min,max = max)

  if (!is.null(which_module) && !is.null(which_pathway)) {
    stop("Specify either modules or pathways, not both.")
  }
  if(is.null(which_pathway) && is.null(which_module)){
    which_module<-seq_len(NumModules)
    attribute_level<-"Panel-level"
    need_module<-which_module
  }else if (!is.null(which_pathway)){
    which_pathway<-validate_pathway_selection(which_pathway = which_pathway,num_pathways = NumPathways)
    attribute_level<-"Pathway-level"
    need_module<-sort(unique(unlist(as.vector(PathwayIndex[PathwayIndex$pathway_index %in% which_pathway,
                                                           1:NumStages]))))
  }else {
    which_module<-validate_module_selection(which_module = which_module,num_modules = NumModules)
    attribute_level<-"Module-level"
    need_module<-which_module
  }
  n_modules<-length(need_module)

  if (!is.null(min) && !is.null(max)) {
    name <- "(range)"
    side <- 2L
  } else if (!is.null(min)) {
    name <- "(min)"
    side <- 1L
  } else {
    name <- "(max)"
    side <- 1L
  }

  x_value<-numeric()
  rhs<-numeric()
  ConstraintMatrix_name <- character()
  col_offsets<-PoolSize*(need_module-1L)
  num_constraints <- n_modules * side * NumStimulus
  i_idx<-seq_len(num_constraints)
  j_items<-rep(pivot_item_id,n_modules)
  j_coloffsets<-rep(col_offsets,each=NumStimulus)
  j_idx<-j_items+j_coloffsets
  j_idx<-rep(j_idx,side)

  if(name=="(range)"){
    min_vec<-rep(min,NumStimulus)
    max_vec<-rep(max,NumStimulus)
    x_value<- c(rep(min_vec,n_modules),
                rep(stimulus_vals,n_modules))
    rhs<-c(rep(stimulus_vals,n_modules),
           rep(max_vec,n_modules))
    ConstraintMatrix_name<-c(paste0("Stimulus ", rep(stim_names,n_modules), " ", attribute,
                                    " ge ", min," when selected in module ",
                                    rep(need_module,each=NumStimulus)),
                             paste0("Stimulus ", rep(stim_names,n_modules), " ", attribute,
                                    " le ", max," when selected in module ",
                                    rep(need_module,each=NumStimulus)))
  }else if(name=="(min)"){
    min_vec<-rep(min,NumStimulus)
    x_value<-rep(min_vec,n_modules)
    rhs<-rep(stimulus_vals,n_modules)
    ConstraintMatrix_name<- paste0("Stimulus ", rep(stim_names,n_modules), " ", attribute,
                                    " ge ", min," when selected in module ",
                                   rep(need_module,each=NumStimulus))

  }else{
    max_vec<-rep(max,NumStimulus)
    x_value<-rep(stimulus_vals,n_modules)
    rhs<-rep(max_vec,n_modules)
    ConstraintMatrix_name<-paste0("Stimulus ", rep(stim_names,n_modules), " ", attribute,
                                  " le ", max," when selected in module ",
                                  rep(need_module,each=NumStimulus))
  }

  ConstraintMatrix <- Matrix::sparseMatrix(i = i_idx, j = j_idx,
                                           x = x_value,
                                           dims = c(num_constraints, num_decisions))
  colnames(ConstraintMatrix)<-paste0("x[", rep(seq_len(NumModules), each = PoolSize), ",", rep(seq_len(PoolSize), NumModules), "]")
  decisionvar_name<-x$decisionvar_name
  if(length(decisionvar_name)!=num_decisions){
    ConstraintMatrix<-ConstraintMatrix[,decisionvar_name,drop = FALSE]
  }
  Specification<-data.frame(a="Quantitative attribute per stimulus",
                            b=attribute,f="Quantitative",c=attribute_level,
                            d = name,e = num_constraints)
  colnames(Specification)<-c("Requirement","Attribute","Type","Application Level","Operator","Num of Constraints")
  Specification$`Num of Constraints`<-as.numeric(Specification$`Num of Constraints`)


  return(create_constraint(name = ConstraintMatrix_name,
                           specification = Specification,
                           A_binary = ConstraintMatrix,A_real = NULL,
                           operators = rep("<=",num_constraints),d = rhs,
                           C_binary = NULL,C_real = NULL))
}
