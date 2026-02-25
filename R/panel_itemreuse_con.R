#' @title Generate Panel-Level Constraints for Item Reuse Across Modules/Pathways
#'
#' @description
#' This function constructs panel-level constraints that limit how many times
#' an item may be selected across the modules and pathways contained within a
#' single MST panel.
#'
#' Two reuse-control modes are supported via \code{overlap}:
#'
#' \itemize{
#'   \item \strong{overlap = FALSE} — No item reuse anywhere in the panel
#'         (the item may appear in at most one module across the entire panel).
#'
#'   \item \strong{overlap = TRUE} — Item reuse is permitted across adaptive
#'         modules within the same stage, but items must remain unique within
#'         each individual pathway.
#' }
#'
#' If \code{item_module_eligibility = NULL} in \code{mst_design()},
#' the total number of constraints depends on \code{overlap}:
#'
#' \itemize{
#'   \item \strong{overlap = FALSE:}
#'
#'  The number of constraints = number of items in the item pool.
#'
#'   \item \strong{overlap = TRUE:}
#'
#'  The number of constraints  = (number of items) × (number of pathways).
#' }
#'
#' @param x An object of class \code{"mstATA_design"} created by \code{mst_design()}.
#' @param overlap Logical.
#'   If \code{FALSE} (default), an item may appear in at most one module in a panel.
#'   If \code{TRUE}, item reuse within the same stage is allowed, but items must be unique within each pathway.
#'
#' @details
#' **1. Specification**
#'
#' The constraint enforces:
#'
#'\strong{An item can be selected at most once within a pathway/panel}.
#'
#' Key characteristics:
#' \itemize{
#'   \item The attribute type is logical (if..., then...) with \emph{item identifier}: each item has an unique ID.
#'   \item The attribute is defined at the \emph{itemset level} (item-itself set) in the item pool.
#'   \item The constraint is enforced at the \strong{Panel-level}, i.e., across all modules and pathways in a panel.
#' }
#'
#' **2. Interpretation of overlap**
#'
#' ### (a) \code{overlap = FALSE} — Strict item uniqueness within the entire panel
#'
#' Under this condition:
#'
#' \itemize{
#'   \item An item may appear in at most one module anywhere in the panel.
#'   \item Selecting the item in any module automatically prevents its selection
#'         in all other modules of the panel.
#'   \item The overlap among pathways are only due to the MST structure.
#' }
#'
#' ### (b) \code{overlap = TRUE} — Item reuse allowed within stage, but not within pathway
#'
#' Under this condition:
#'
#' \itemize{
#'   \item Items **may** be reused across adaptive modules in the same stage.
#'   \item Items **must not** be reused within the same pathway (i.e., an item may appear at most once per pathway).
#'   \item The overlap among pathways not only due to the MST structure, but also due to some common items
#'   across modules in the same stage.
#' }
#'
#' ### Example interpretation for a 1-3-3 design:
#'
#' \itemize{
#'   \item (1) If an item is selected in the routing module → it cannot appear in any S2 or S3 module.
#'
#'   \item (2) If selected in an S2 module → it may appear in other S2 modules (same stage),
#'         but not in S1R or any S3 module.
#'
#'   \item (3) If selected in an S3 module → it may appear in other S3 modules,
#'         but not in S1R or any S2 module.
#' }
#'
#' **3. Why This Is a Panel-Level Constraint**
#'
#' Although the constraint logic differs for the two overlap conditions, both
#' versions govern item allocation across the set of modules that comprise a
#' full panel. In other words, the constraint concerns **how an item may or may
#' not appear within the assembled panel as a whole**, rather than applying to a
#' specific module or pathway in isolation. For this reason, both cases are
#' formally categorized as *Panel-level* item-reuse constraints.
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
#' **Case 1: \code{overlap = TRUE}**
#' Item must be unique within each pathway:
#'
#' \deqn{
#'   \sum_{m \in r} x_{m,i_s} \le 1
#' }
#'
#' **Case 2: \code{overlap = FALSE}**
#' Item must be unique across the entire panel:
#'
#' \deqn{
#'   \sum_{m} x_{m,i_s} \le 1
#' }
#'
#' Here:
#' \itemize{
#'   \item \eqn{x_{m,i_s}} is the binary decision variable indicating whether
#'   item \eqn{i_s} is selected into module m.
#'
#'   \item \eqn{m \in r} denote modules belonging to pathway r.
#' }
#'
#' @return An object of S3 class \code{"constraint"} with named elements:
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
#'
#' @examples
#' data("mini_itempool")
#'
#' test_mstATA <- mst_design(
#'   itempool      = mini_itempool,
#'   design        = "1-3-3",
#'   module_length = c(4,2,2,2,2,2,2)
#' )
#'
#' # Example 1: No item overlap within a panel
#' panel_itemreuse_con(x = test_mstATA, overlap = FALSE)
#'
#' # Example 2: Allow item reuse within the same stage
#' panel_itemreuse_con(x = test_mstATA, overlap = TRUE)
#'
#'
#' # Example 3: item_module_eligibility supplied — disjoint module eligibility
#' # No need for panel_itemreuse_con() in this case
#' item_module_eligibility <- list(
#'   `1` = intersect(which(mini_itempool$difficulty >= -1),
#'                   which(mini_itempool$difficulty < 0)),
#'   `2` = intersect(which(mini_itempool$difficulty >= -2),
#'                   which(mini_itempool$difficulty < -1)),
#'   `3` = intersect(which(mini_itempool$difficulty >= 0),
#'                   which(mini_itempool$difficulty < 1)),
#'   `4` = intersect(which(mini_itempool$difficulty >= 1),
#'                   which(mini_itempool$difficulty < 2))
#' )
#'
#' new_test_mstATA <- mst_design(
#'   itempool               = mini_itempool,
#'   design                 = "1-3",
#'   module_length          = c(6,7,7,7),
#'   item_module_eligibility = item_module_eligibility
#' )
#'
#' ## The generated constraint matrix are redundant and will be omitted in the ATA model.
#' # panel_itemreuse_con(x = new_test_mstATA, overlap = FALSE)
#'
#'
#' # Example 4: Overlapping module eligibility — reuse constraints required
#' item_module_eligibility <- list(
#'   `1` = which(mini_itempool$difficulty < 2),
#'   `2` = which(mini_itempool$difficulty < 0),
#'   `3` = intersect(which(mini_itempool$difficulty >= -1),
#'                   which(mini_itempool$difficulty < 1)),
#'   `4` = intersect(which(mini_itempool$difficulty >= 0),
#'                   which(mini_itempool$difficulty < 2))
#' )
#'
#' new_test_mstATA <- mst_design(
#'   itempool               = mini_itempool,
#'   design                 = "1-3",
#'   module_length          = c(6,7,7,7),
#'   item_module_eligibility = item_module_eligibility
#' )
#'
#' # Items eligible for multiple modules require reuse control
#' panel_itemreuse_con(x = new_test_mstATA, overlap = FALSE)
#'
#' # Overlap within stage → item may appear in more modules
#' panel_itemreuse_con(x = new_test_mstATA, overlap = TRUE)
#'
#'
#' @export
panel_itemreuse_con <- function(x,overlap = FALSE) {
  if (!inherits(x, "mstATA_design")) {
    stop("Input x must be an object of class 'mstATA_design'.")
  }
  PoolSize <- nrow(x$ItemPool)
  NumStages <- x$NumStages
  NumModules <- x$NumModules
  NumPathways <- x$NumPathways
  PathwayIndex <- x$PathwayIndex
  num_decisions <- PoolSize * NumModules
  decisionvar_name<-x$decisionvar_name

  i_idx <-integer()
  j_idx <-integer()
  ConstraintMatrix_name<-character()

  if(overlap){
    which_pathway<-1:NumPathways
    modules_involved<-vapply(which_pathway,FUN = function(pathway_id) {
      as.vector(as.matrix(PathwayIndex[PathwayIndex$pathway_index==pathway_id,1:NumStages]))
    },FUN.VALUE = integer(NumStages))
    col_offset<-apply(modules_involved,MARGIN = 2,FUN = function(module_id) (module_id-1L)*PoolSize)
    col_offset_full<-as.vector(col_offset)
    i_idx<-c(i_idx,rep(seq_len(PoolSize*NumPathways),each = NumStages))
    j_items<-rep(seq_len(PoolSize),each=NumStages*NumPathways)
    j_idx<-c(j_idx,j_items+rep(col_offset_full,PoolSize))
    num_constraints <- PoolSize*NumPathways

    ConstraintMatrix_name<-c(ConstraintMatrix_name,
                             paste("The number of times using Item",rep(1:PoolSize,each = NumPathways),"in Pathway",1:NumPathways))
    Specification<-data.frame(a="Item exposure control within a panel",
                              b="Item_id",
                              c="Logical",d="Panel-level",
                              e = "(max)",f = num_constraints)
  }else{
    which_module<-1:NumModules
    i_idx<-c(i_idx,rep(seq_len(PoolSize),each = NumModules))
    col_offset<-PoolSize*(which_module-1L)
    j_items<-rep(seq_len(PoolSize),each=NumModules)
    j_idx<-c(j_idx,j_items+rep(col_offset,PoolSize))
    num_constraints<-PoolSize

    ConstraintMatrix_name<-c(ConstraintMatrix_name,
                             paste("The number of time using Item",1:PoolSize,"across",NumModules,"Modules"))
    Specification<-data.frame(a="Item exposure control within a panel",
                              b="Item_id (item-itself set)",
                              c="Logical",d="Panel-level",
                              e = "(max)",f = num_constraints)

  }

  ConstraintMatrix <- Matrix::sparseMatrix(i = i_idx,j = j_idx,x = 1L,dims = c(num_constraints,num_decisions))
  colnames(ConstraintMatrix)<-paste0("x[", rep(seq_len(NumModules), each = PoolSize), ",", rep(seq_len(PoolSize), NumModules), "]")
  if(length(decisionvar_name)!=num_decisions){
    ConstraintMatrix<-ConstraintMatrix[,decisionvar_name,drop=FALSE]
    keep_rows<-which(apply(ConstraintMatrix,1,sum)>1)
    if(length(keep_rows)==0){
      stop("All constraints removed due to item-module eligibility restrictions in mst_design()")
    }
    ConstraintMatrix<-ConstraintMatrix[keep_rows,,drop=FALSE]
    ConstraintMatrix_name<-ConstraintMatrix_name[keep_rows]
    num_constraints<-nrow(ConstraintMatrix)
  }

  colnames(Specification)<-c("Requirement","Attribute","Type","Application Level","Operator","Num of Constraints")
  Specification$`Num of Constraints`<-as.numeric(Specification$`Num of Constraints`)
  Specification$`Num of Constraints`<-num_constraints

  return(create_constraint(name = ConstraintMatrix_name,specification = Specification,
                           A_binary = ConstraintMatrix,A_real = NULL,
                           operators = rep("<=",num_constraints),d = rep(1,num_constraints),
                           C_binary = NULL,C_real = NULL))
}


