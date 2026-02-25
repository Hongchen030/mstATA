#' @title Generate Pathway-Level Constraints to Prevent Enemy Stimuli
#'   from Appearing Together
#'
#' @description
#'
#' Constructs linear constraints to ensure that stimuli belonging to the
#' same *enemy set* do **not** appear together within any MST pathway (i.e.,
#' any complete test form).
#'
#' Enemy stimuli are stimuli that should not be viewed by the same examinee for security or fairness reasons.
#'
#' The total number of constraints = number of enemy stimulus sets * number of pathways.
#'
#' @param x An object of class `"mstATA_design"` created by `mst_design()`.
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
#'
#' **1. Specification**
#'
#' The enforced constraint is:
#'
#' \strong{At most one stimulus from the same enemy set can be selected in a pathway}
#'
#' \itemize{
#'   \item The attribute is a \emph{itemset-level logical grouping}: enemystim_set defines enemy stimulus groups in the
#'   item pool.
#'   \item The constraint must be applied at \strong{"Pathway-level"}.
#'   Because each pathway represents the full sequence of modules that an examinee
#'   could encounter, preventing enemy stimuli from appearing in a pathway ensures
#'  that no examinee can see more than one stimulus from the same enemy set.
#' }
#'
#' **2. Logical Interpretation**
#'
#' For an enemy stimulus set \eqn{V_e^{stim}}:If a stimulus in the set is selected anywhere in the pathway,
#' all other stimuli in the same set must be excluded from that pathway.
#'
#' This ensures that **at most one** stimulus from each enemy group appears in any
#' assembled test form.
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
#' Let \eqn{V_e^{stim}} denotes a set of stimuli that are mutually exclusive (enemy).
#'
#' \deqn{
#'   \sum_{m \in r} \sum_{s \in V_e^{stim}} x_{i_s^{*},m} \;\le\; 1 .
#' }
#'
#' Here:
#'
#' \itemize{
#'   \item \eqn{x_{i_s^{*},m}} indicates whether the pivot item for stimulus
#'   s is selected into module m, thereby indicating whether the
#'   stimulus s is selected in that module.
#'   \item \eqn{m \in r} denote modules belonging to pathway r.
#' }
#'
#' @seealso
#' [create_enemy_sets()],
#' [concat_enemy_sets()],
#' [create_pivot_stimulus_map()]
#' @examples
#' data("reading_itempool")
#'
#' pivot_stim_map <- create_pivot_stimulus_map(
#'   itempool   = reading_itempool,
#'   stimulus   = "stimulus",
#'   pivot_item = "pivot_item"
#' )
#'
#' # Create enemy-stimulus sets
#' enemystim_set<-create_enemy_sets(
#'   reading_itempool$stimulus,
#'   reading_itempool$enemy_stimulus,
#'   sep_pattern = ","
#' )
#'
#' test_mstATA <- mst_design(
#'   itempool       = reading_itempool,
#'   design         = "1-3-3",
#'   module_length  = c(10,12,12,12,15,15,15),
#'   exclude_pathway = c("1-1-3", "1-3-1"),
#'   pivot_stim_map = pivot_stim_map,
#'   enemystim_set = enemystim_set
#' )
#'
#' enemystim_exclu_con(x = test_mstATA)
#'
#' @export

enemystim_exclu_con <- function(x) {
  if (!inherits(x, "mstATA_design")) {
    stop("Input 'x' must be an object of class 'mstATA_design'.")
  }

  enemystim_set <- x$enemystim_set

  if (is.null(enemystim_set)) {
    stop("Enemy stimulus constraints require `enemystim_set`. ",
         "Prepare it with `create_enemy_set()`/`concat_enemy_set()` and supply it via `mst_design()`.")
  }

  enemy_sets<-enemystim_set$EnemySet
  NumSets<-length(enemy_sets)
  ItemIndex<-enemystim_set$ItemIndex

  ItemPool<-x$ItemPool
  PoolSize <- nrow(ItemPool)
  PathwayIndex <- x$PathwayIndex
  NumModules <- x$NumModules
  NumPathways <- x$NumPathways
  NumStages <- x$NumStages
  which_pathway<-1:NumPathways
  num_decisions<-PoolSize*NumModules

  modules_involved<-vapply(which_pathway,FUN = function(pathway_id) {
    as.vector(as.matrix(PathwayIndex[PathwayIndex$pathway_index==pathway_id,1:NumStages]))
  },FUN.VALUE = integer(NumStages))
  col_offset<-apply(modules_involved,MARGIN = 2,FUN = function(module_id) (module_id-1L)*PoolSize)
  col_offsets<-as.vector(col_offset)

  i_idx<-integer()
  j_idx<-integer()
  ConstraintMatrix_name<-character()
  for(set_id in 1:NumSets){
    pivot_pos<-ItemIndex[[set_id]]
    set_start<-1+NumPathways*(set_id-1L)
    set_end<-NumPathways*set_id
    i_idx<-c(i_idx,rep(set_start:set_end,each=length(pivot_pos)*NumStages))
    j_item<-rep(rep(pivot_pos,NumStages),NumPathways)
    col_offset_full<-rep(col_offsets,each=length(pivot_pos))
    j_idx<-c(j_idx,j_item+col_offset_full)
    ConstraintMatrix_name<-c(ConstraintMatrix_name,
                             paste("Do not include",paste(enemy_sets[[set_id]],collapse = ", "),
                                   "together in Pathway",which_pathway))
  }
  num_constraints<-NumPathways*NumSets
  Specification <- data.frame(Requirement="Enemy stimulus exclusion",Attribute = "Enemy stimuli membership",
                              Type = "Logical",Level = "Pathway-level",
                              Operator = "(max)",`Num of Constraints` = num_constraints,
                              stringsAsFactors = FALSE)
  colnames(Specification)<-c("Requirement","Attribute","Type","Application Level","Operator","Num of Constraints")

  ConstraintMatrix<-Matrix::sparseMatrix(i = i_idx,j = j_idx,x = 1L,dims = c(num_constraints,num_decisions))
  operators<-rep("<=",num_constraints)
  rhs<-rep(1,num_constraints)
  decisionvar_name<-x$decisionvar_name
  colnames(ConstraintMatrix)<-paste0("x[", rep(seq_len(NumModules), each = PoolSize), ",", rep(seq_len(PoolSize), NumModules), "]")
  if(length(decisionvar_name)!=num_decisions){
    ConstraintMatrix<-ConstraintMatrix[,decisionvar_name,drop = FALSE]
  }

  return(create_constraint(name = ConstraintMatrix_name,
                           specification = Specification,
                           A_binary  = ConstraintMatrix,A_real = NULL,
                           d = rhs,operators = operators,
                           C_binary = NULL,C_real = NULL))
}


