#' @title Generate Pathway-Level Constraints to Prevent Enemy Items
#' from Appearing Together
#'
#' @description
#' Constructs linear constraints to ensure that items belonging to the
#' same *enemy set* do **not** appear together within any assembled MST
#' pathway (i.e., any complete test form).
#'
#' Enemy items are items that:
#' \itemize{
#'   \item give away the answer to one another,
#'   \item are too similar in content or structure, or
#'   \item should not be seen by the same examinee for security or fairness reasons.
#' }
#'
#' The total number of constraints = num of enemy item sets * num of pathways
#'
#' @param x An object of class `"mstATA_design"` created by \code{mst_design()}.
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
#' The enforced constraint is:
#'
#' \strong{At most one item from the same enemy set can be selected in a pathway}
#'
#' \itemize{
#'   \item The attribute is a \emph{itemset-level logical grouping}: enemyitem_set defines enemy item groups in the
#'   item pool.
#'   \item The constraint must be applied at \strong{"Pathway-level"}. Because each pathway represents the full sequence of modules that an examinee
#'   could encounter, preventing enemy items from appearing in a pathway ensures
#'  that no examinee can see more than one item from the same enemy set.
#' }
#'
#' **2. Logical Interpretation**
#'
#' For an enemy item set \eqn{V_e^{item}}:If one item in the set is selected anywhere in the pathway,
#' all other items in the same set must be excluded from that pathway.
#'
#' This ensures that **at most one** item from each enemy group appears in any
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
#' Let \eqn{V_e^{item}} denotes a set of items that are mutually exclusive (enemy) items.
#'
#' \deqn{
#'   \sum_{m \in r} \sum_{i_s \in V_e^{item}} x_{i_s,m} \;\le\; 1 .
#' }
#'
#' Here:
#'
#' \itemize{
#'   \item \eqn{x_{i_s,m}} is the binary decision variable indicating whether
#'   item \eqn{i_s} is selected into module m.
#'   \item \eqn{m \in r} denote modules belonging to pathway r.
#' }
#'
#' @examples
#' # Example 1: Two enemy sets: {Item1, Item2, Item3} and {Item4, Item5}
#' data("mini_itempool")
#'
#' enemyitem_set<- create_enemy_sets(mini_itempool$item_id,
#'                                  mini_itempool$enemy_similarity,
#'                                  sep_pattern = ",")
#'
#' test_mstATA <- mst_design(
#'   itempool = mini_itempool,
#'   design = "1-3-3",
#'   module_length = c(4,2,2,2,2,2,2),
#'   enemyitem_set = enemyitem_set
#' )
#'
#' enemyitem_exclu_con(x = test_mstATA)
#'
#'
#' # Example 2:
#' ## Enemy relationships based on similarity
#' similarity <- create_enemy_sets(
#'   id_col    = mini_itempool$item_id,
#'   enemy_col = mini_itempool$enemy_similarity
#' )
#'
#' ## Enemy relationships based on cluing
#' cluing <- create_enemy_sets(
#'   id_col    = mini_itempool$item_id,
#'   enemy_col = mini_itempool$enemy_cluing
#' )
#'
#' ## Combine all enemy relationships into a single enemy set
#' enemy_all <- concat_enemy_sets(
#'   similarity,
#'   cluing
#' )
#'
#' ## Use the combined enemy set in the test design
#' test_mstATA2 <- mst_design(
#'   itempool       = mini_itempool,
#'   design = "1-3-3",
#'   module_length = c(4,2,2,2,2,2,2),
#'   enemyitem_set = enemy_all
#' )
#'
#' enemyitem_exclu_con(test_mstATA2)
#'
#' @seealso [create_enemy_sets()],[concat_enemy_sets()]
#' @export

enemyitem_exclu_con <- function(x) {
  if (!inherits(x, "mstATA_design")) {
    stop("Input 'x' must be an object of class 'mstATA_design'.")
  }

  enemyitem_set <- x$enemyitem_set

  if (is.null(enemyitem_set)) {
    stop("Enemy item constraints require `enemyitem_set`. ",
         "Prepare it with `create_enemy_sets()`/`concat_enemy_sets()` and supply it via `mst_design()`.")
  }

  enemy_sets<-enemyitem_set$EnemySet
  NumSets<-length(enemy_sets)
  ItemIndex<-enemyitem_set$ItemIndex

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
    items_pos<-ItemIndex[[set_id]]
    n_items<-length(items_pos)
    set_start<-1+NumPathways*(set_id-1L)
    set_end<-NumPathways*set_id
    i_idx<-c(i_idx,rep(set_start:set_end,each = n_items*NumStages))
    j_item<-rep(rep(items_pos,NumStages),NumPathways)
    col_offset_full<-rep(col_offsets,each=n_items)
    j_idx<-c(j_idx,j_item+col_offset_full)
    ConstraintMatrix_name<-c(ConstraintMatrix_name,
                             paste("Do not include",paste(enemy_sets[[set_id]],collapse = ", "),
                                   "together in Pathway",which_pathway))
  }
  num_constraints<-NumPathways*NumSets
  Specification <- data.frame(a = "Enemy item exclusion",b="Enemy items membership",
                              c = "Logical",d = "Pathway-level",
                              e = "(max)",f = num_constraints,
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

