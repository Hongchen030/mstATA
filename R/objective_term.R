#' @title Create a Single Linear Objective Term
#'
#' @description
#' Creates a linear objective term of the form \eqn{a^\top x}, where the vector
#' \eqn{a} represents categorical or quantitative attribute values for the
#' involved items (or stimuli), and the decision variables \eqn{x} represent
#' item selections within a single MST panel.
#'
#' Depending on the attribute supplied, the objective can target:
#' \itemize{
#'   \item maximize or minimize the number of items belong to a specific category level, or
#'   \item maximize or minimize the sum of quantitative attribute values across items
#'                 (e.g., difficulty, IIF values, discrimination).
#' }
#'
#' Objective terms may be applied at three hierarchical levels:
#' \itemize{
#'   \item \strong{"Module-level"} — aggregates only items belonging to one
#'         specific module;
#'
#'   \item \strong{"Pathway-level"} — aggregates items in all modules that compose
#'         a single MST pathway; and
#'
#'   \item \strong{"Panel-level"} — aggregates all item selections across all
#'         modules in a panel.
#' }
#'
#' The function supports two types of objectives:
#' \itemize{
#'   \item \strong{Relative objective} – maximize or minimize \eqn{a^\top x}.
#'   \item \strong{Absolute deviation objective} – minimize \eqn{|a^\top x - \text{goal}|}.
#' }
#'
#' @param x An object of class `"mstATA_design"` created by [mst_design()].
#' @param attribute A string giving the column name in `x$ItemPool` that contains
#'   the attribute used in the objective term. This may be categorical or
#'   quantitative.
#' @param cat_level A character string or `NULL`.
#'   If a character string is supplied, the attribute is treated as categorical
#'   and the objective counts items belonging to this category.
#'   If `NULL`, the attribute is treated as quantitative and the objective uses
#'   the numeric attribute values.
#' @param applied_level One of:
#'   `"Module-level"`, `"Pathway-level"`, `"Panel-level"`.
#'   See *Details* for the rules governing module/pathway selection.
#' @param which_module Integer scalar specifying the module to which the
#'   objective applies (only permitted when `applied_level = "Module-level"`).
#' @param which_pathway Integer scalar specifying the pathway to which the
#'   objective applies (only permitted when `applied_level = "Pathway-level"`).
#' @param sense One of `"max"` or `"min"`.
#'   For absolute deviation objectives, `"min"` is enforced automatically.
#' @param goal Optional numeric scalar. If supplied, the objective minimizes the
#'   absolute deviation from the target:
#'   \deqn{ |a^\top x - \text{goal}| }
#'   If `NULL`, a standard maximize/minimize objective is created.
#'
#' @details
#'
#' **1. Application scope**
#'
#' \itemize{
#'
#'   \item **Module-level:**
#'     \code{which_module} must be supplied and must be a scalar.
#'     \code{which_pathway} must be \code{NULL}.
#'
#'   \item **Pathway-level:**
#'     \code{which_pathway} must be supplied and must be a scalar.
#'     \code{which_module} must be \code{NULL}.
#'
#'   \item **Panel-level:**
#'     Neither \code{which_module} nor \code{which_pathway} may be supplied.
#'
#' }
#'
#' **2. Objective meaning**
#'
#' If `cat_level = NULL`, the vector \eqn{a} consists of quantitative attribute
#' values.
#' If `cat_level` is specified, \eqn{a} is an indicator vector for items in the
#' corresponding category.
#'
#' The objective term therefore represents:
#'
#' \itemize{
#'   \item the \strong{sum of attribute values} (quantitative), or
#'   \item the \strong{count of items} in a category (categorical).
#' }
#'
#' @return
#' An object of class `"mstATA_objective"` with components:
#' \describe{
#'   \item{name}{Character description of the objective term.}
#'   \item{coef_val}{A 1-row sparse matrix containing the objective coefficients.}
#'   \item{goal}{The target value for absolute deviation objectives, or `NULL`.}
#'   \item{sense}{The optimization direction (`"max"` or `"min"`).}
#' }
#' @examples
#' data("mini_itempool")
#' test_mstATA <- mst_design(
#'   itempool       = mini_itempool,
#'   design         = "1-3-3",
#'   exclude_pathway = c("1-1-3","1-3-1"),
#'   pathway_length = 8
#' )
#'
#' # Example 1: Maximize information at $\theta$ = 0 in the routing module
#' objective_term(
#'   x = test_mstATA,
#'   attribute = "iif(theta=0)",
#'   cat_level = NULL,
#'   applied_level = "Module-level",
#'   which_module = 1,
#'   sense = "max"
#' )
#'
#' # Example 2: Target a test information value at $\theta$ = -1 for pathway 1
#' objective_term(
#'   x = test_mstATA,
#'   attribute = "iif(theta=-1)",
#'   cat_level = NULL,
#'   applied_level = "Pathway-level",
#'   which_pathway = 1,
#'   sense = "max",
#'   goal = 10
#' )
#'
#' # Example 3: Maximize the number of MC items in a panel
#' objective_term(
#'   x = test_mstATA,
#'   attribute = "itemtype",
#'   cat_level = "MC",
#'   applied_level = "Panel-level",
#'   sense = "max"
#' )
#'
#' @export
#'
objective_term <- function(x,attribute,cat_level = NULL,
                           applied_level = c("Module-level","Pathway-level","Panel-level"),
                           which_module = NULL,which_pathway = NULL,
                           sense = "max",goal = NULL) {
  if (!inherits(x, "mstATA_design")) {
    stop("Input 'x' must be an object of class 'mstATA_design'.")
  }
  ItemPool<-x$ItemPool
  PoolSize<-nrow(ItemPool)
  if(is.null(cat_level)){
    name<- sprintf("the sum of %s values", attribute)
    item_vals<-get_attribute_val(itempool = ItemPool,attribute = attribute,cat_level = cat_level)
  }else {
    name<-sprintf("the number of items from '%s' category", cat_level)
    item_vals<-get_attribute_val(itempool = ItemPool,attribute = attribute,cat_level = cat_level)
  }
  sense <- match.arg(sense,c("max","min"))

  if(is.null(goal)){
    if(sense=="max"){
      type<-paste("Relative Objective: Maximize",name)
    }else{
      type<-paste("Relative Objective: Minimize",name)
    }
  }else if(all(is.numeric(goal),length(goal)==1L,is.finite(goal))){
    if(sense=="max"){
      message("Numeric target supplied; switching sense to 'min' for absolute deviation objective.")
      sense<-"min"
    }
    type<-paste("Absolute Objective: Minimize the absolute deviation between",name,"and the target value of",goal)
  }else{
    stop("Invalid 'goal'. Must be NULL or a single finite numeric value.")
  }


  NumModules<-x$NumModules
  NumStages<-x$NumStages
  NumPathways<-x$NumPathways
  PathwayIndex<-x$PathwayIndex
  num_decisions<-NumModules*PoolSize
  scope<-match.arg(applied_level,c("Module-level","Pathway-level","Panel-level"))
  validated<-validate_scope(applied_level = scope,which_module = which_module,which_pathway = which_pathway,
                            num_modules = NumModules,num_pathways = NumPathways)
  which_module<-validated$which_module
  which_pathway<-validated$which_pathway

  ConstraintMatrix_name<-character()

  if (scope == "Panel-level") {
    i_idx <- rep.int(1L, PoolSize * NumModules)
    col_offset <- (seq_len(NumModules) - 1L) * PoolSize
    j_idx<-rep(seq_len(PoolSize),NumModules)+rep(col_offset,each=PoolSize)
    x_value<-rep(item_vals,NumModules)
    ConstraintMatrix_name<-paste(type,"in a panel")
  }else if (scope == "Module-level") {
    i_idx <- rep.int(1L, PoolSize)
    j_idx <- (which_module - 1L) * PoolSize + seq_len(PoolSize)
    x_value<-item_vals
    ConstraintMatrix_name<-paste(type,"in Module",which_module)
  }else{
    modules_involved <- unlist(PathwayIndex[PathwayIndex$pathway_index == which_pathway, 1:NumStages])
    col_offset<-PoolSize*(modules_involved-1L)
    i_idx <- rep.int(1L, PoolSize * NumStages)
    j_idx<-rep(seq_len(PoolSize),NumStages)+rep(col_offset,each=PoolSize)
    x_value<-rep(item_vals,NumStages)
    ConstraintMatrix_name<-paste(type,"in Pathway",which_pathway)
  }
  ConstraintMatrix<-Matrix::sparseMatrix(i = i_idx,j = j_idx,x = x_value,dims = c(1,num_decisions))
  colnames(ConstraintMatrix)<-paste0("x[", rep(seq_len(NumModules), each = PoolSize), ",", rep(seq_len(PoolSize), NumModules), "]")
  decisionvar_name<-x$decisionvar_name
  if(length(decisionvar_name)!=num_decisions){
    ConstraintMatrix<-ConstraintMatrix[,decisionvar_name,drop=FALSE]
  }
  structure(list(name = ConstraintMatrix_name,
                 coef_val = ConstraintMatrix,
                 goal = goal,
                 sense = sense),
            class = "mstATA_objective")
}







