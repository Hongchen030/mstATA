#' @title Generate Item-Level Constraints to Explicitly Select or Omit Specific Items
#'
#' @description
#' This function generates linear constraints that require specific items to be
#' either must be **selected** or **not selected** in a multistage test (MST) panel
#' assembly.
#'
#' If \code{item_module_eligibility} is supplied in \code{mst_design()} and the
#' requested item selection conflicts with module eligibility restrictions, an
#' error is thrown.
#' For details about \code{item_module_eligibility}, see \code{mst_design()}.
#'
#' Constraints may be applied at the `"Module-level"`, `"Pathway-level"`,
#' or `"Panel-level"`.
#'
#' If \code{item_module_eligibility} is \code{NULL}, the number of constraints is:
#'
#' \itemize{
#'   \item \strong{Module-level:} when \code{which_module} is provided and
#'         \code{which_pathway = NULL}
#'
#'  The number of constraints is
#'     \strong{(number of items in \code{item_ids}) × (number of specified modules)}
#'
#'   \item \strong{Pathway-level:} when \code{which_pathway} is provided and
#'         \code{which_module = NULL}
#'
#'  The number of constraints is
#'     \strong{(number of items in \code{item_ids}) × (number of specified pathways)}
#'
#'   \item \strong{Panel-level:} both \code{which_module} and \code{which_pathway} are \code{NULL}
#'
#'  The number of constraints is
#'     \strong{(number of items in \code{item_ids})}
#' }
#'
#' @param x An object of class `"mstATA_design"` created by `mst_design()`.
#' @param item_ids A numeric vector of item row indices, or
#'   a character vector of item IDs from \code{x$ItemPool}.
#' @param select Logical. Indicates whether items in \code{item_ids} must be
#'   \code{TRUE} (selected) or \code{FALSE} (not selected). Default = \code{TRUE}.
#' @param which_module Optional integer vector of module indices to which the constraints
#'   apply.
#' @param which_pathway Optional integer vector of pathway indices to which the
#'   constraints apply.
#'
#' @details
#' **1. Specification**
#'
#' The constraint enforced by this function is:
#'
#' \strong{Items listed in \code{item_ids} must (or must not) be selected.}
#'
#' Key characteristics:
#' \itemize{
#'   \item The attribute type is categorical (each item has a unique ID).
#'   \item The attribute is defined at the item-level in the item pool.
#'   \item Application levels:
#'     \itemize{
#'       \item \strong{Module-level}: enforce selection in specific module(s).
#'       \item \strong{Pathway-level}: enforce selection in any module of a pathway.
#'       \item \strong{Panel-level}: enforce selection somewhere in the panel.
#'     }
#' }
#'
#' **2. Examples of Interpretation**
#'
#' \itemize{
#'
#'   \item \strong{Item i must be selected in the routing module} (Module-level):
#'     \code{item_ids = i}, \code{which_module = 1}
#'
#'   \item \strong{Item i must be selected in Stage 2 easy and medium modules}
#'     (modules not belonging to the same pathway) (Module-level):
#'     \code{which_module = c(2,3)}
#'
#'   \item \strong{Item i must be selected in the RM pathway} (Pathway-level):
#'     \code{which_pathway = 2}
#'
#'   \item \strong{Item i must appear somewhere in the assembled panel}
#'     (Panel-level):
#'     \code{which_module = NULL}, \code{which_pathway = NULL}
#'
#' }
#'
#' **3. Interaction with \code{panel_itemreuse_con()}**
#'
#' When \code{overlap = FALSE} in \code{panel_itemreuse_con()}, each item can at most be selected once
#' in a panel.
#'
#' Therefore:
#'
#' \itemize{
#'   \item \code{which_module} or \code{which_pathway} must be a scalar (not
#'   a vector).
#'   \item If both are \code{NULL}, the constraint simply enforces that the
#'         item must appear somewhere in the panel, without specifying the
#'         module/pathway.
#' }
#'
#' When \code{overlap = TRUE} in \code{panel_itemreuse_con()}, each item can at most be selected once
#' in a pathway, but each item may be selected in multiple modules within the same stage.
#'
#' Therefore:
#'
#' \itemize{
#'   \item \code{which_module} may be a vector (can not be modules that appear in the same pathway)
#' }
#'
#'
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
#' **1. Module-level item selection/not selection**
#'
#' In specified module m: for all \eqn{i_s} specified in \code{item_ids}
#'
#' \deqn{
#'   x_{i_s, m} = 1 \quad (\text{selected}), \qquad
#'   x_{i_s, m} = 0 \quad (\text{not selected}), \qquad
#' }
#'
#' **2. Pathway-level item selection/not selection**
#'
#' In specified pathway r:  for all \eqn{i_s} specified in \code{item_ids}
#'
#' \deqn{
#'   \sum_{m \in r} x_{i_s, m} = 1 \quad (\text{selected}), \qquad
#'   \sum_{m \in r} x_{i_s, m} = 0 \quad (\text{not selected}), \qquad
#' }
#'
#'
#' **3. Panel-level item selection/not selection**
#'
#' In a panel: for all \eqn{i_s} specified in \code{item_ids}
#'
#' \deqn{
#'   \sum_{m} x_{i_s, m} = 1 \quad (\text{selected}), \qquad,
#'   \sum_{m} x_{i_s, m} = 0 \quad (\text{not selected}), \qquad
#' }
#'
#' Here:
#' \itemize{
#'   \item \eqn{x_{i_s, m}} is the binary decision variable indicating whether
#'   item \eqn{i_s} is selected into module m.
#'
#'   \item \eqn{m \in r} denote modules belonging to pathway r.
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
#' data("mini_itempool")
#' test_mstATA <- mst_design(
#'   itempool = mini_itempool,
#'   design = "1-3-3",
#'   module_length = c(4,2,2,2,2,2,2)
#' )
#'
#' # Example 1: Item 2 must be selected in the routing module
#' itemcat_con(x = test_mstATA, item_ids = 2, select = TRUE, which_module = 1)
#'
#' # Example 2: Item 2 must appear somewhere in the MST panel
#' itemcat_con(x = test_mstATA, item_ids = 2, select = TRUE)
#'
#' # Example 3: Item 2 must be selected in Stage 2 easy and medium modules
#' itemcat_con(x = test_mstATA, item_ids = 2, select = TRUE, which_module = c(2,3))
#'
#' # Example 4: Item 2 must be selected in the REE pathway
#' itemcat_con(x = test_mstATA, item_ids = 2, select = TRUE, which_pathway = 1)
#'
#' # Example 5: Items 1 and 3 must be selected in the routing module
#' itemcat_con(x = test_mstATA, item_ids = c(1,3), select = TRUE, which_module = 1)
#'
#' # Example 6: Items 1 and 3 must be selected in the REE pathway
#' itemcat_con(x = test_mstATA, item_ids = c(1,3), select = TRUE, which_pathway = 1)
#'
#' # Example 7: Items 1 and 3 must appear somewhere in the panel
#' itemcat_con(x = test_mstATA, item_ids = c(1,3), select = TRUE)
#'
#'
#' # Example 8: With item_module_eligibility provided
#' mini_itempool <- mini_itempool[order(mini_itempool$difficulty), ]
#' item_module_eligibility <- list(
#'   `2` = 1:15,
#'   `3` = 11:25,
#'   `4` = 15:30
#' )
#'
#' new_test_mstATA <- mst_design(
#'   itempool = mini_itempool,
#'   design = "1-3-3",
#'   module_length = c(6,4,4,4,3,3,3),
#'   item_module_eligibility = item_module_eligibility
#' )
#' itemcat_con(new_test_mstATA, item_ids = 2, select = TRUE, which_module = 2)
#' # error: item 2 is not eligible for the selection in module 3.
#' # itemcat_con(new_test_mstATA, item_ids = 2, select = TRUE, which_module = c(2,3))
#'
#'
#' @export


itemcat_con <- function(x, item_ids, select = TRUE,
                        which_module=NULL,which_pathway=NULL) {

  if (!inherits(x, "mstATA_design")) {
    stop("Input 'x' must be an object of class 'mstATA_design'.")
  }
  ItemPool <- x$ItemPool
  item_id_col<-x$item_id_col
  PoolSize <- nrow(ItemPool)
  NumStages <-x$NumStages
  NumModules <- x$NumModules
  NumPathways <- x$NumPathways
  PathwayIndex<-x$PathwayIndex
  num_decisions<-PoolSize*NumModules
  decisionvar_name<-x$decisionvar_name

  index_map <- setNames(seq_len(PoolSize), ItemPool[[item_id_col]])
  item_index<-check_item_ids(item_ids = item_ids,item_names = names(index_map))
  n_items<-length(item_index)

  if (!is.null(which_module) && !is.null(which_pathway)) {
    stop("Specify either modules or pathways, not both.")
  }
  if(is.null(which_pathway) & is.null(which_module)){
    which_module<-1:NumModules
    attribute_level<-"Panel-level"
    need_module<-1:NumModules
  }else if (!is.null(which_pathway)){
    which_pathway<-validate_pathway_selection(which_pathway = which_pathway,num_pathways = NumPathways)
    attribute_level<-"Pathway-level"
    need_module<-sort(unlist(unique(as.vector(PathwayIndex[PathwayIndex$pathway_index %in% which_pathway,
                                                           1:NumStages]))))
  }else {
    which_module<-validate_module_selection(which_module = which_module,num_modules = NumModules)
    attribute_level<-"Module-level"
    need_module<-which_module
  }

  wanted_decision<-paste0("x[", rep(need_module, each = length(item_index)), ",", rep(item_index, length(need_module)), "]")

  if (select && any(!wanted_decision %in% decisionvar_name)) {
    bad <- wanted_decision[!wanted_decision %in% decisionvar_name]
    stop(
      "Conflict with item_module_eligibility: ",
      paste(bad, collapse = ", "),
      " are not decision variables in mst_design()."
    )
  }

  if(select){name<-"Select"}else{name<-"Not select"}
  i_idx<-integer()
  j_idx<-integer()
  ConstraintMatrix_name <- character()


  if(attribute_level=="Module-level"){
    n_modules<-length(which_module)
    num_constraints<-n_modules*n_items
    col_offset <- PoolSize * (which_module- 1L)
    i_idx <- seq_len(num_constraints)
    j_idx <- rep(item_index,n_modules) + rep(col_offset,each = n_items)
    ConstraintMatrix_name<- paste(name,
                                  paste0("item ", rep(item_index, n_modules)," in module ", rep(which_module, each = n_items)))
  }else if(attribute_level=="Pathway-level"){
    n_pathways<-length(which_pathway)
    num_constraints<-n_pathways*n_items
    modules_involved<-vapply(which_pathway,FUN = function(pathway_id) {
      as.vector(as.matrix(PathwayIndex[PathwayIndex$pathway_index==pathway_id,1:NumStages]))
    },FUN.VALUE = integer(NumStages))
    col_offset<-apply(modules_involved,MARGIN = 2,FUN = function(module_id) (module_id-1L)*PoolSize)
    col_offset_full<-as.vector(col_offset)
    i_idx<-rep(seq_len(num_constraints),each=NumStages)
    j_idx<-rep(item_index,each = NumStages*n_pathways)+rep(col_offset_full,length(item_index))
    ConstraintMatrix_name<-paste(name,
                                 paste0("item ",rep(item_index,each=n_pathways)," in pathway ",rep(which_pathway,length(item_index))))
  }else{
    num_constraints<-n_items
    col_offset <- PoolSize * (which_module- 1L)
    i_idx <- rep(seq_len(n_items),each=NumModules)
    j_idx <- rep(item_index,each = NumModules)+rep(col_offset,n_items)
    ConstraintMatrix_name <- paste(name,paste0("item ",item_index," in a panel."))
  }
  ConstraintMatrix <- Matrix::sparseMatrix(i = i_idx, j = j_idx, x = 1L, dims = c(num_constraints, num_decisions))
  colnames(ConstraintMatrix)<-paste0("x[", rep(seq_len(NumModules), each = PoolSize), ",", rep(seq_len(PoolSize), NumModules), "]")
  if(length(decisionvar_name)!=num_decisions){
    ConstraintMatrix<-ConstraintMatrix[,decisionvar_name,drop = FALSE]
    keep_rows<-which(apply(ConstraintMatrix,1,sum)!=0)
    if (length(keep_rows) == 0) {
      stop("All constraints removed due to item-module eligibility restrictions in mst_design().")
    }
    ConstraintMatrix<-ConstraintMatrix[keep_rows,,drop=FALSE]
    ConstraintMatrix_name<-ConstraintMatrix_name[keep_rows]
    num_constraints<-nrow(ConstraintMatrix)
  }

  Specification<-data.frame(a=paste(name,"item",paste(item_index,collapse = "/")),b="Item_id",
                            c="Categorical",d=attribute_level,
                            e = "(exact)",f = num_constraints)
  colnames(Specification)<-c("Requirement","Attribute","Type","Application Level","Operator","Num of Constraints")
  Specification$`Num of Constraints`<-as.numeric(Specification$`Num of Constraints`)
  operators <- rep("=",num_constraints)

  if (select) {
    rhs <- rep(1,num_constraints)
  } else {
    rhs <- rep(0,num_constraints)
  }
  return(create_constraint(name = ConstraintMatrix_name,
                           specification = Specification,
                           A_binary = ConstraintMatrix,A_real = NULL,
                           operators = rep("=",num_constraints),d = rhs,
                           C_binary = NULL,C_real = NULL))
}

