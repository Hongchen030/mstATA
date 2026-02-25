#' @title Generate Module/Pathway-Level Constraints on the Sum of Item Quantitative Attributes
#'
#' @description
#' This function generates linear constraints on the **sum of item-level quantitative
#' attributes** (e.g., total time, word count, item difficulty) within specified
#' module or pathway of a multistage test (MST).
#'
#' Users may specify:
#' \itemize{
#'   \item minimum total item-level attribute values,
#'   \item maximum total item-level attribute values, or
#'   \item exact (equal-to) total item-level attribute values.
#' }
#'
#' Constraints may apply at the:
#' \itemize{
#'   \item \strong{"Module-level"} — each module is treated as a separate test form, or
#'   \item \strong{"Pathway-level"} — each pathway is treated as a complete test form.
#' }
#'
#'
#' The total number of constraints depends on the application level:
#'
#' \itemize{
#'   \item \strong{Module-level:} when \code{which_module} is provided
#'          OR both \code{which_module} and \code{which_pathway} are \code{NULL}
#'
#'    The number of constraints is
#'     \strong{(number of modules specified)}
#'
#'   \item \strong{Pathway-level:} when \code{which_pathway} is provided and
#'         \code{which_module = NULL}
#'
#'    The number of constraints is
#'     \strong{(number of pathways specified)}
#' }
#'
#' @param x An object of class \code{"mstATA_design"} created by \code{mst_design()}.
#' @param attribute A string giving the column name in \code{x$ItemPool} containing
#'   the **item quantitative attribute** (e.g., time, difficulty, word count).
#' @param operator A character string specifying the constraint type.
#'   Must be one of \code{"<="}, \code{">="}, or \code{"="}.
#'   For two-sided (range) constraints, use \code{test_itemquant_range_con()}.
#' @param target_value A numeric scalar or vector giving the target sum of item
#'   quantitative attributes for each module/pathway being constrained.
#' @param which_module Optional integer vector of module indices to which the constraints
#'   apply.
#' @param which_pathway Optional integer vector of pathway indices to which the
#'   constraints apply.
#'
#' @details
#' **1. Specification**
#'
#' The constraint enforced is:
#'
#' \strong{The total sum of the item quantitative attribute must meet a
#' minimum, exact, or maximum bound for specific module or pathway.}
#'
#' Key characteristics:
#'
#' \itemize{
#'   \item The attribute type is \emph{quantitative}.
#'   \item The attribute is defined at the \emph{item level} in the item pool.
#'   \item The constraints are applied at either \strong{"Module-level"} or \strong{"Pathway-level"}.
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
#' Let \eqn{q_{i_s}} be the quantitative attribute value of item \eqn{i_s}.
#'
#' **1. Module-level constraint (for module m)**
#'
#' \deqn{
#' \sum_{s=1}^{S} \sum_{i_s=1}^{I_s} q_{i_s} x_{i_s,m}
#' \;\; \substack{\le \\ \ge \\ =} \;\;
#' b_{q}^{item,m}
#' }
#'
#' **2. Pathway-level constraint (for pathway r)**
#'
#' \deqn{
#' \sum_{m \in r} \sum_{s=1}^{S} \sum_{i_s=1}^{I_s} q_{i_s} x_{i_s,m}
#' \;\; \substack{\le \\ \ge \\ =} \;\;
#' b_{q}^{item,r}
#' }
#'
#' Here:
#'
#' \itemize{
#'   \item \eqn{x_{i_s,m}} is the binary decision variable indicating whether
#'   item \eqn{i_s} is selected into module m.
#'
#'   \item \eqn{m \in r} denote modules belonging to pathway r.
#'
#'   \item The stacked operator, \eqn{\substack{\leq \\ \geq \\ =}} denotes that the specification may take the form of an upper bound, lower bound, or exact value.
#'
#'   \item \eqn{b_{q}^{item,m}} is the required bound for the sum of item-level quantitative attribute values in module m (minimum, maximum, or exact value).
#'
#'   \item \eqn{b_{q}^{item,r}} is the required bound for the sum of item-level quantitative attribute values in pathway r (minimum, maximum, or exact value).
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
#' data("mini_itempool")
#'
#' # Example 1: Upper bound on total difficulty per module
#' test_mstATA <- mst_design(
#'   itempool = mini_itempool,
#'   design = "1-3-3",
#'   module_length = c(4,2,2,2,2,2,2)
#' )
#'
#' # Mean difficulty upper bound per module:
#' #   difficulty_mean * module_length = difficulty_sum_target
#' test_itemquant_con(
#'   x = test_mstATA,
#'   attribute = "difficulty",
#'   operator = "<=",
#'   target_value = c(0, -0.5, 0, 0.5, -1, 0, 1) * c(4,2,2,2,2,2,2),
#'   which_module = NULL
#' )
#'
#'
#' # Example 2: Difficulty boundaries for different pathways
#' # Assume three subpopulations: low, medium, high ability.
#' # Pathways targeting low ability: mean difficulty <= -0.44
#' # Pathways targeting high ability: mean difficulty >= 0.44
#'
#' test_mstATA <- mst_design(
#'   itempool = mini_itempool,
#'   design = "1-3-3",
#'   exclude_pathways = c("1-1-3","1-3-1"),
#'   pathway_length = 8
#' )
#'
#' # Low-difficulty pathways
#' test_itemquant_con(
#'   x = test_mstATA,
#'   attribute = "difficulty",
#'   operator = "<=",
#'   target_value = -0.44 * 8,
#'   which_pathway = c(1,2)
#' )
#'
#' # High-difficulty pathways
#' test_itemquant_con(
#'   x = test_mstATA,
#'   attribute = "difficulty",
#'   operator = ">=",
#'   target_value = 0.44 * 8,
#'   which_pathway = c(6,7)
#' )
#'
#' @export


test_itemquant_con <- function(x, attribute, operator, target_value,
                                            which_module=NULL,which_pathway=NULL) {
  if (!inherits(x, "mstATA_design")) {
    stop("Input 'x' must be an object of class 'mstATA_design'.")
  }
  ItemPool<-x$ItemPool

  item_vals <- get_attribute_val(itempool = ItemPool,attribute = attribute,cat_level = NULL)
  operator<-check_operator(operator = operator)
  if(operator==">="){
    name<-"(min)"
  }else if(operator=="<="){
    name<-"(max)"
  }else{
    name<-"(exact)"
  }

  PoolSize <- nrow(ItemPool)
  ModuleIndex <- x$ModuleIndex
  PathwayIndex <- x$PathwayIndex
  NumStages <- x$NumStages
  NumModules <- x$NumModules
  NumPathways<-x$NumPathways
  num_decisions<-PoolSize*NumModules

  check_scope<-check_test_constraint_scope(num_modules = NumModules,num_pathways = NumPathways,
                                           which_module = which_module, which_pathway = which_pathway)
  application_level<-check_scope[["application_level"]]
  which_module <- check_scope[["which_module"]]
  which_pathway <- check_scope[["which_pathway"]]
  total_rows<-check_scope[["total_rows"]]
  row_ids<-check_scope[["row_ids"]]

  target_value<-expand_target_to_matrix(input = target_value,levels = NULL,
                                        total_rows = total_rows,row_ids = row_ids)

  i_idx <- integer()
  j_idx <- integer()
  x_value<-numeric()
  rhs <- target_value[,1]
  ConstraintMatrix_name<-c()

  if (application_level == "Module-level") {
    num_constraints <- length(which_module)
    i_idx<-c(i_idx,rep(seq_len(num_constraints),each=PoolSize))
    col_offset<-(which_module-1L)*PoolSize
    j_idx<-c(j_idx,(seq_len(PoolSize)+rep(col_offset,each = PoolSize)))
    x_value<-rep(item_vals,num_constraints)
    ConstraintMatrix_name<-c(ConstraintMatrix_name,
                             paste("The",name,"sum of",attribute,"in Module",which_module))
    Specification<-data.frame(a=paste0("Sum of ",attribute),
                              b=attribute,c="Quantitative",d="Module-level",e=name,f=num_constraints,
                              stringsAsFactors = FALSE)
    } else {
    num_constraints <-length(which_pathway)
    i_idx<-c(i_idx,rep(seq_len(num_constraints),each=PoolSize*NumStages))
    module_list <- lapply(which_pathway, function(pathway_id) {
      as.vector(as.matrix(
        PathwayIndex[PathwayIndex$pathway_index == pathway_id, 1:NumStages]
      ))
    })
    col_offset<-PoolSize*(unlist(module_list)-1L)
    j_idx<-c(j_idx,(seq_len(PoolSize)+rep(col_offset,each = PoolSize)))
    x_value<-rep(rep(item_vals,NumStages),num_constraints)
    ConstraintMatrix_name<-c(ConstraintMatrix_name,
                             paste("The",name,"sum of",attribute,"in Pathway",which_pathway))
    Specification<-data.frame(a=paste0("Sum of ",attribute),
                              b=attribute,c="Quantitative",d="Pathway-level",e=name,f=num_constraints,
                              stringsAsFactors = FALSE)
    }
  colnames(Specification)<-c("Requirement","Attribute","Type","Application Level","Operator","Num of Constraints")
  Specification$`Num of Constraints`<-as.numeric(Specification$`Num of Constraints`)

  ConstraintMatrix<- Matrix::sparseMatrix(i = i_idx, j = j_idx, x = x_value,
                                          dims = c(num_constraints, num_decisions))

  decisionvar_name<-x$decisionvar_name
  colnames(ConstraintMatrix)<-paste0("x[", rep(seq_len(NumModules), each = PoolSize), ",", rep(seq_len(PoolSize), NumModules), "]")
  if(length(decisionvar_name)!=num_decisions){
    ConstraintMatrix<-ConstraintMatrix[,decisionvar_name]
  }

  return(create_constraint(name = ConstraintMatrix_name,
                           specification = Specification,
                           A_binary = ConstraintMatrix,A_real = NULL,
                           operators = rep(operator,num_constraints),d = rhs,
                           C_binary = NULL,C_real = NULL))
}

