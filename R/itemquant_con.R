#' @title Generate Item-Level Constraints Requiring Quantitative Attributes
#'   to Satisfy Lower, Upper, or Range Bounds (Not for Operational Use)
#'
#' @description
#' This function constructs linear constraints enforcing that every selected item
#' satisfies a quantitative attribute requirement (e.g., difficulty ≥ c, item
#' length ≤ c, discrimination within a range).
#'
#' Constraints may be applied at the `"Module-level"`, `"Pathway-level"`, or
#' `"Panel-level"`.
#'
#' **This function is for demonstration only and is *not intended for operational use*.**
#' It generates an
#' excessively large number of rows in the constraint matrix and requires
#' item-level attributes to be strictly positive.
#'
#' The number of generated linear constraints depends on the application level:
#'
#' \itemize{
#'   \item \strong{Module-level:} when \code{which_module} is provided
#'
#'  The number of constraints is
#'     \strong{(number of items) × (number of modules specified) × side}
#'
#'   \item \strong{Pathway-level:} when \code{which_pathway} is provided and
#'         \code{which_module = NULL}
#'
#'  The number of constraints is
#'     \strong{(number of items) × (number of unique modules in specified pathways) × side}
#'
#'   \item \strong{Panel-level:} both \code{which_module} and \code{which_pathway} are \code{NULL}
#'
#'  The number of constraints is
#'     \strong{(number of items) × (total number of modules) × side}
#' }
#'
#' where \code{side = 1} for one-sided constraints (min-only or max-only)
#' and \code{side = 2} when both \code{min} and \code{max} are provided and
#' \code{min ≠ max}.
#'
#' @param x An object of class `"mstATA_design"` created by `mst_design()`.
#' @param attribute A string giving the column name in \code{x$ItemPool} that
#'   represents the **item-level quantitative attribute**.
#' @param min A numeric scalar for the lower bound.
#' @param max A numeric scalar for the upper bound.
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
#' \strong{Each selected item must satisfy the specified quantitative
#' lower bound, upper bound, or both.}
#'
#' Key characteristics:
#' \itemize{
#'   \item The attribute type is \emph{quantitative}.
#'   \item The attribute is defined at the \emph{item level} in the item pool.
#'   \item Application levels:
#'     \itemize{
#'       \item \strong{Module-level}: enforce items selected in specific module(s) satisfy the quantitative requirement.
#'       \item \strong{Pathway-level}: enforce items selected in any module of a pathway satisfy the quantitative requirement.
#'       \item \strong{Panel-level}: enforce items selected in the panel satisfy the quantitative requirement.
#'     }
#' }
#'
#' **2. Important Restriction**
#'
#' There are two reasons this approach is discouraged. First, item-level
#' quantitative constraints generate an excessively large number of
#' constraint rows, leading to poor computational efficiency. Second, the
#' formulation requires scaling item-selection variables by the attribute
#' value \eqn{q_{i_s}}. If \eqn{q_{i_s}} is zero or negative, the resulting
#' inequalities become invalid.
#'
#' Because many commonly used item attributes (e.g., IRT difficulty parameters
#' or centered text indices) may take zero or negative values,
#' \strong{this function is not suitable for operational MST assembly}.
#'
#' Instead, per-item quantitative rules should be enforced during the design
#' stage using \code{item_module_eligibility()} within \code{mst_design()},
#' which excludes ineligible items prior to module assembly.
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
#' Upper-bound constraint:
#' \deqn{
#'   q_{i_s} \, x_{i_s, m} \le b_q^{item,max}
#' }
#'
#' Lower-bound constraint:
#' \deqn{
#'   b_q^{item,min} \, x_{i_s, m} \le q_{i_s}
#' }
#'
#' For two-sided range constraints, both inequalities are included.
#'  (\code{min} and \code{max} both provided and not
#' equal), the number of constraints is doubled (\code{side = 2}).
#' For one-sided constraints, \code{side = 1}.
#'
#' Here:
#' \itemize{
#'   \item \eqn{x_{i_s, m}} is the binary decision variable indicating whether
#'   item \eqn{i_s} is selected into module m.
#'
#'  \item \eqn{q_{i_s}} (**must be positive values**) denote the values of a quantitative attribute for item \eqn{i_s}.
#'
#'  \item \eqn{b_q^{item,min}} and \eqn{b_q^{item,max}} are the lower and
#' upper allowable bounds for the attribute \eqn{q_{i_s}}.
#' }
#'
#' @return
#' This function always throws an error indicating that it is
#' \strong{not for operational use}.
#'
#' It is retained solely as an illustrative example of how item-level
#' quantitative constraints propagate through different hierarchical levels
#' of an MST design.
#'
#' @seealso
#' \code{\link{mst_design}}
#'
#' @export
#'

itemquant_con<-function(x,attribute,min = NULL,max = NULL,
                        which_module=NULL,which_pathway=NULL){

  if (!inherits(x, "mstATA_design")) {
    stop("Input 'x' must be an object of class 'mstATA_design'.")
  }
  stop(
    "'itemquant_con()' is deprecated. It is retained for documentation and internal demonstration only ",
    "and should not be used operationally.\n\n",
    "For per-item quantitative requirements, use 'item_module_eligibility' in ",
    "'mst_design()' to filter eligible items before assembly."
  )

  ItemPool<-x$ItemPool
  item_vals <- check_attribute_column(itempool = ItemPool,attribute = attribute)
  if(any(item_vals<=0)){
    stop("This function assumes quantitative attribute values are strictly positive.")
  }
  NumModules<-x$NumModules
  NumPathways<-x$NumPathways
  NumStages<-x$NumStages
  PoolSize<-nrow(ItemPool)
  PathwayIndex<-x$PathwayIndex
  num_decisions<-PoolSize*NumModules
  decisionvar_name<-x$decisionvar_name

  if (is.null(min) && is.null(max)) {
    stop("At least one of 'min','max' should be provided.")
  }
  validate_min_max(min = min,max = max)

  if(!is.null(which_module) && !is.null(which_pathway)){
    stop("Specify either 'which_module' or 'which_pathway', not both.")
  }
  if(is.null(which_pathway) & is.null(which_module)){
    which_module<-1:NumModules
    attribute_level<-"Panel-level"
  }else if (!is.null(which_pathway)){
    which_pathway<-validate_pathway_selection(which_pathway = which_pathway,num_pathways = NumPathways)
    attribute_level<-"Pathway-level"
    which_module<-sort(unlist(unique(as.vector(PathwayIndex[PathwayIndex$pathway_index %in% which_pathway,
                                                     1:NumStages]))))
  }else {
    which_module<-validate_module_selection(which_module = which_module,num_modules = NumModules)
    attribute_level<-"Module-level"
  }

  if (!is.null(min) && !is.null(max)) {
    name <- "lower and upper bound"
    side <- 2L
  } else if (!is.null(min)) {
    name <- "lower bound"
    side <- 1L
  } else {
    name <- "upper bound"
    side <- 1L
  }

  x_value<-numeric()
  ConstraintMatrix_name <- character()
  col_offset<-PoolSize*(which_module-1L)
  n_modules<-length(which_module)
  num_constraints <- n_modules * side * PoolSize
  i_idx<-seq_len(num_constraints)
  j_idx<-seq_len(PoolSize)+rep(col_offset,each=PoolSize)
  j_idx<-rep(j_idx,side)

  if(name=="lower and upper bound"){
    x_value<- c(rep(min,n_modules*PoolSize),
                rep(item_vals,n_modules))
    rhs<-c(rep(item_vals,n_modules),
           rep(max,n_modules*PoolSize))
    ConstraintMatrix_name<-c(paste0("Item ",1:PoolSize," ",attribute," larger than ",min,
                                    " when selected in module ",rep(which_module,each=PoolSize),"?"),
                             paste0("Item ",1:PoolSize," ",attribute," smaller than ",max,
                                    " when selected in module ",rep(which_module,each=PoolSize),"?"))
  }else if(name=="lower bound"){
    x_value<- rep(min,n_modules*PoolSize)
    rhs<-rep(item_vals,n_modules)
    ConstraintMatrix_name<-paste0("Item ",1:PoolSize," ",attribute," larger than ",min,
                                  " when selected in module ",rep(which_module,each=PoolSize),"?")
  }else{
    x_value<- rep(item_vals,n_modules)
    rhs<-rep(max,n_modules*PoolSize)
    ConstraintMatrix_name<-paste0("Item ",1:PoolSize," ",attribute," smaller than ",max,
                                  " when selected in module ",rep(which_module,each=PoolSize),"?")
  }

  ConstraintMatrix <- Matrix::sparseMatrix(i = i_idx, j = j_idx,
                                           x = x_value,
                                           dims = c(num_constraints, num_decisions))
  colnames(ConstraintMatrix)<-paste0("x[", rep(seq_len(NumModules), each = PoolSize), ",", rep(seq_len(PoolSize), NumModules), "]")

  if(length(decisionvar_name)!=num_decisions){
    ConstraintMatrix<-ConstraintMatrix[,decisionvar_name,drop = FALSE]

    temp_name <- paste0("x[", rep(which_module, each = PoolSize), ",", rep(seq_len(PoolSize), n_modules), "]")
    if(side == 1L){
      rownames(ConstraintMatrix)<-temp_name
      which_in<-which(temp_name%in%decisionvar_name)
      ConstraintMatrix<-ConstraintMatrix[which_in,,drop = FALSE]
      rownames(ConstraintMatrix)<-NULL
    }else{
      half<-num_constraints/2
      half1<-ConstraintMatrix[1:half,,drop=FALSE]
      half2<-ConstraintMatrix[(1+half):num_constraints,,drop=FALSE]
      rownames(half1)<-rownames(half2)<-temp_name
      which_inhalf<-which(temp_name%in%decisionvar_name)
      which_in<-c(which_inhalf,half+which_inhalf)
      ConstraintMatrix<-ConstraintMatrix[which_in,,drop=FALSE]
    }
    ConstraintMatrix_name<-ConstraintMatrix_name[which_in]
    num_constraints<-nrow(ConstraintMatrix)
  }

  Specification<-data.frame(a="Every selected item must satisfy the quantitative attribute requirement.",
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

