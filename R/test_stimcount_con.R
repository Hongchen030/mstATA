#' @title Generate Module/Pathway-Level Constraints for the Number of Selected Stimuli
#'
#' @description
#' This function generates linear constraints on the *number of stimuli* selected
#' in a multistage test (MST).
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
#' The total number of constraints is:
#' \itemize{
#'   \item \strong{Module-level:} when \code{which_module} is provided, or when both
#'     \code{which_module} and \code{which_pathway} are \code{NULL}
#'
#'   The number of constraints is
#'     \strong{(number of modules specified)}
#'
#'   \item \strong{Pathway-level:} when \code{which_pathway} is provided and \code{which_module = NULL}
#'
#'   The number of constraints is
#'     \strong{(number of pathways specified)}
#' }
#'
#'
#' @param x An object of class `"mstATA_design"` created by `mst_design()`.
#' @param operator A character string indicating the type of constraint.
#'   Must be one of \code{"<="}, \code{"="}, or \code{">="}.
#' @param target_num The number of stimuli in specific module/pathway (\code{which_module}, \code{which_pathway}).
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
#' \strong{The test must meet a minimum, exact, or maximum number of stimuli.}
#'
#' Key characteristics:
#'
#' \itemize{
#'   \item The attribute type is \emph{categorical} (each stimulus has a unique ID).
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
#'
#' **1. Module-level constraint (for module m)**
#'
#' \deqn{\sum_{s=1}^{S-1} x_{i_s^{*},m} \;\substack{\le \\ \ge \\ =}\; n_m}
#'
#' **2. Pathway-level constraint (for pathway r)**
#'
#' \deqn{\sum_{m \in r} \sum_{s=1}^{S-1} x_{i_s^{*},m} \;\substack{\le \\ \ge \\ =}\; n_r}
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
#'   \item \eqn{n_m} specify the required number of stimuli in module m.
#'
#'   \item \eqn{n_r} specify the required number of stimuli in pathway r.
#' }
#'
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
#' @references
#' van der Linden, W. J. (2005). *Linear Models for Optimal Test Design.*
#' Springer. https://doi.org/10.1007/0-387-29054-0
#'
#' @examples
#' data("reading_itempool")
#'
#' pivot_stim_map <- create_pivot_stimulus_map(
#'   itempool = reading_itempool,
#'   stimulus = "stimulus",
#'   pivot_item = "pivot_item"
#' )
#'
#' test_mstATA <- mst_design(
#'   itempool = reading_itempool,
#'   design = "1-3-3",
#'   module_length = c(14,12,12,12,12,12,12),
#'   pivot_stim_map = pivot_stim_map
#' )
#'
#' # Example 1:
#' # In MST 1-3-3, the routing module must contain between 1 and 2 stimuli.
#' con1<-test_stimcount_con(
#'     x = test_mstATA,
#'     operator = ">=",
#'     target_num = 1,
#'     which_module = 1
#'   )
#' con2<-test_stimcount_con(
#'     x = test_mstATA,
#'     operator = "<=",
#'     target_num = 2,
#'     which_module = 1
#'   )
#'
#'
#' # Example 2:
#' # The REE pathway must contain exactly 2 stimuli.
#' test_stimcount_con(
#'   x = test_mstATA,
#'   operator = "=",
#'   target_num = 2,
#'   which_pathway = 1
#' )
#'
#' @export
test_stimcount_con<-function(x,operator,target_num,
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
  NumStages <- x$NumStages
  NumModules <- x$NumModules
  NumPathways<-x$NumPathways
  PoolSize <-nrow(ItemPool)
  PathwayIndex <- x$PathwayIndex
  num_decisions<-PoolSize*NumModules

  pivot_item_ids     <- pivot_stim_map$pivot_item_id

  check_scope<-check_test_constraint_scope(num_modules = NumModules,num_pathways = NumPathways,
                                           which_module = which_module, which_pathway = which_pathway)
  application_level<-check_scope[["application_level"]]
  which_module <- check_scope[["which_module"]]
  which_pathway <- check_scope[["which_pathway"]]
  total_rows<-check_scope[["total_rows"]]
  row_ids<-check_scope[["row_ids"]]
  num_constraints<-length(row_ids)

  target_num<-expand_target_to_matrix(input = target_num,levels = NULL,total_rows = total_rows,
                                      row_ids = row_ids)
  check_nonneg_integer(target_num,"target_num")
  rhs<-target_num[,1]
  operators<-rep(operator,num_constraints)
  i_idx <- integer()
  j_idx <- integer()
  ConstraintMatrix_name<-character()

  if (application_level == "Module-level") {
    i_idx <- c(i_idx,rep(seq_len(num_constraints),each=length(pivot_item_ids)))
    col_offset <- PoolSize*(which_module-1L)
    j_idx <-c(j_idx,c(pivot_item_ids+rep(col_offset,each=length(pivot_item_ids))))
    ConstraintMatrix_name<-c(ConstraintMatrix_name,
                             paste("The",name,"number of stimuli in Module",which_module))
    Specification<-data.frame(a="Stimulus count",b="Stimulus_id",
                              c="Categorical",d=application_level,e=name,f=num_constraints)
  }else{
    all_j<-lapply(which_pathway,function(pathway_id){
      modules <- as.vector(as.matrix(PathwayIndex[PathwayIndex$pathway_index == pathway_id, 1:NumStages]))
      unlist(lapply(modules,function(module_id) pivot_item_ids+(module_id-1L)*PoolSize))
      })
    i_idx <- c(i_idx,rep(seq_len(num_constraints),each =length(pivot_item_ids)*NumStages))
    j_idx <- c(j_idx,unlist(all_j))
    ConstraintMatrix_name<-c(ConstraintMatrix_name,
                             paste("The",name,"number of stimuli in Pathway",which_pathway))
    Specification<-data.frame(a="Stimulus count",
                              b="Stimulus_id",
                              c="Categorical",d=application_level,e= name,f=num_constraints)
  }
  colnames(Specification)<-c("Requirement","Attribute","Type","Application Level","Operator","Num of Constraints")
  Specification$`Num of Constraints`<-as.numeric(Specification$`Num of Constraints`)
  ConstraintMatrix<- Matrix::sparseMatrix(i = i_idx, j = j_idx, x = 1L,
                                          dims = c(num_constraints, num_decisions))

  decisionvar_name<-x$decisionvar_name
  colnames(ConstraintMatrix)<-paste0("x[", rep(seq_len(NumModules), each = PoolSize), ",", rep(seq_len(PoolSize), NumModules), "]")
  if(length(decisionvar_name)!=num_decisions){
    ConstraintMatrix<-ConstraintMatrix[,decisionvar_name]
  }

  return(create_constraint(name = ConstraintMatrix_name,specification = Specification,
                           A_binary = ConstraintMatrix,A_real = NULL,
                           operators = operators,d = rhs,
                           C_binary = NULL,C_real = NULL))
}

