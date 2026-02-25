#' @title Generate Module/Pathway-Level Constraints for the Sum of Stimulus Quantitative Attributes
#'
#' @description
#' This function generates linear constraints on the **sum of stimulus-level quantitative
#' attribute** (e.g., total word count, total reading time,
#' total linguistic complexity) in a multistage test (MST).
#'
#' Users may specify:
#' \itemize{
#'   \item minimum total stimulus-level quantitative attribute values,
#'   \item maximum total stimulus-level quantitative attribute values, or
#'   \item exact (equal-to) total stimulus-level attribute values.
#' }
#'
#' Constraints may be applied at:
#' \itemize{
#'   \item \strong{"Module-level"} — each module is treated as an independent form.
#'   \item \strong{"Pathway-level"} — each pathway is treated as an independent form.
#' }
#'
#' The total number of constraints depends on the application level:
#'
#' \itemize{
#'   \item \strong{Module-level:}when \code{which_module} is provided
#'          OR both \code{which_module} and \code{which_pathway} are \code{NULL}
#'
#'    The number of constraints is
#'     \strong{(number of modules specified)}
#'
#'   \item \strong{Pathway-level:}when \code{which_pathway} is provided and
#'         \code{which_module = NULL}
#'
#'     The number of constraints is
#'     \strong{(number of pathways specified)}
#' }
#'
#' @param x An object of class \code{"mstATA_design"} created by \code{mst_design()}.
#' @param attribute A string giving the column name in \code{x$ItemPool} that
#'   represents the **stimulus-level quantitative attribute**.
#' @param operator A character string indicating the type of constraint.
#'   Must be one of \code{"<="}, \code{"="}, or \code{">="}.
#' @param target_value A numeric scalar or vector specifying the required total
#'   quantitative attribute value.
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
#' \strong{The total value of quantitative attribute across selected stimuli must meet a minimum, exact, or maximum requirement.}
#'
#' Example:In a stimulus-based assessment, total words in each module must not exceed 300.
#'
#' Key characteristics:
#' \itemize{
#'   \item The attribute type is \emph{quantitative}.
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
#' Let \eqn{q_s} be the quantitative attribute value of stimulus s.
#'
#' **1. Module-level constraint (for module m)**
#'
#' \deqn{
#' \sum_{s=1}^{S-1} q_s x_{i_s^{*},m}
#' \;\; \substack{\le \\ \ge \\ =} \;\;
#' b_{q}^{stim,m}
#' }
#'
#' **2. Pathway-level constraint (for pathway r)**
#'
#' \deqn{
#' \sum_{m \in r} \sum_{s=1}^{S-1} q_s x_{i_s^{*},m}
#' \;\; \substack{\le \\ \ge \\ =} \;\;
#' b_{q}^{stim,r}
#' }
#'
#' Here:
#'
#' \itemize{
#'   \item \eqn{x_{i_s^{*},m}} indicates whether the pivot item for stimulus
#'   s is selected into module m, thereby indicating whether the
#'   stimulus is selected in that module.
#'
#'   \item \eqn{m \in r} denote modules belonging to pathway r.
#'
#'   \item The stacked operator, \eqn{\substack{\leq \\ \geq \\ =}} denotes that the specification may take the form of an upper bound, lower bound, or exact value.
#'
#'   \item \eqn{b_{q}^{stim,m}} is the required bound for the sum of stimulus-level quantitative attribute values in module m.
#'
#'   \item \eqn{b_{q}^{stim,r}} is the required bound for the sum of stimulus-level quantitative attribute values in pathway r.
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
#' pivot_stim_map <- create_pivot_stimulus_map(reading_itempool,
#'                                             item_id_col="item_id",
#'                                             stimulus = "stimulus",
#'                                              pivot_item = "pivot_item")
#' test_mstATA <- mst_design(
#'   itempool = reading_itempool,
#'   design = "1-3-3",
#'   module_length = c(14,12,12,12,12,12,12),
#'   pivot_stim_map = pivot_stim_map
#' )
#'
#' # Example 1:
#' # The total words in each module must be ≤ 300.
#' test_stimquant_con(
#'   x = test_mstATA,
#'   attribute = "stimulus_words",
#'   operator = "<=",
#'   target_value = 300,
#'   which_module = NULL
#' )
#'
#'
#' # Example 2:
#' # In the REE pathway, total stimulus words must be between 50 and 200.
#' con1<-test_stimquant_con(
#'     x = test_mstATA,
#'     attribute = "stimulus_words",
#'     operator = ">=",
#'     target_value = 50,
#'     which_pathway = 1
#'   )
#' con2<-test_stimquant_con(
#'     x = test_mstATA,
#'     attribute = "stimulus_words",
#'     operator = "<=",
#'     target_value = 200,
#'     which_pathway = 1
#'   )
#'
#'
#' @export


test_stimquant_con <- function(x,attribute,
                               operator,target_value,
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

  ItemPool<-x$ItemPool
  PoolSize<-nrow(ItemPool)
  NumStages <- x$NumStages
  NumModules <- x$NumModules
  NumPathways<-x$NumPathways
  PathwayIndex<-x$PathwayIndex
  num_decisions<-PoolSize*NumModules

  pivot_item_ids     <- pivot_stim_map$pivot_item_id
  stim_names <- pivot_stim_map$stimulus_name
  NumStimulus<-length(pivot_item_ids)
  stimulus_vals <- check_attribute_column(itempool = ItemPool[pivot_item_ids,],attribute = attribute)

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
  total_rows<-check_scope[["total_rows"]]
  row_ids<-check_scope[["row_ids"]]

  target_value<-expand_target_to_matrix(input = target_value,levels = NULL,
                                        total_rows = total_rows,row_ids = row_ids)

  i_idx<-integer()
  j_idx<-integer()
  rhs <- target_value[,1]
  ConstraintMatrix_name<-character()
  x_value<-numeric()

  if(application_level=="Module-level"){
    num_constraints<-length(which_module)
    col_offset<-PoolSize*(which_module-1L)
    i_idx<-c(i_idx,rep(seq_along(which_module),each=NumStimulus))
    j_idx<-c(j_idx,(pivot_item_ids+rep(col_offset,each=NumStimulus)))
    x_value<-rep(stimulus_vals,num_constraints)
    ConstraintMatrix_name<-c(ConstraintMatrix_name,
                             paste("The",name,"sum of",attribute,"in Module",which_module))
    Specification<-data.frame(a= paste0("Sum of ",attribute),
                              b=attribute,c="Quantitative",d="Module-level",e=name,f=num_constraints,
                              stringsAsFactors = FALSE)

  }else {
    num_constraints <-length(which_pathway)
    i_idx<-c(i_idx,rep(seq_along(which_pathway),each=NumStimulus*NumStages))
    modules_involved<-vapply(which_pathway,FUN = function(pathway_id) {
      as.vector(as.matrix(PathwayIndex[PathwayIndex$pathway_index==pathway_id,1:NumStages]))
    },FUN.VALUE = integer(NumStages))
    modules_involved_vec<-as.vector(modules_involved)
    col_offset<-PoolSize*(modules_involved_vec-1L)
    j_idx<-c(j_idx,(pivot_item_ids+rep(col_offset,each=NumStimulus)))
    x_value<-rep(stimulus_vals,num_constraints*NumStages)
    ConstraintMatrix_name<-c(ConstraintMatrix_name,
                             paste("The",name,"sum of",attribute,"in Pathway",which_pathway))
    Specification<-data.frame(a=paste0("Sum of ",attribute),
                              b=attribute,c="Quantitative",d="Pathway-level",e=name,f=num_constraints,
                              stringsAsFactors = FALSE)
  }
  colnames(Specification)<-c("Requirement","Attribute","Type","Application Level","Operator","Num of Constraints")
  Specification$`Num of Constraints`<-as.numeric(Specification$`Num of Constraints`)

  ConstraintMatrix<-Matrix::sparseMatrix(i = i_idx,j = j_idx,x = x_value,dims = c(num_constraints,num_decisions))

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

