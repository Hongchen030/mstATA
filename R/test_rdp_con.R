#' Routing Decision Point Information Balance Constraint
#'
#' Enforces similarity of test information at routing decision points (RDPs)
#' between adjacent modules in the *next stage* of a multistage test (MST).
#' Routing is assumed to occur **after examinees complete `which_stage`**,
#' and constraints are imposed on modules at stage `which_stage + 1`.
#'
#' For each routing decision point \eqn{\theta_{rdp}}, this function constrains
#' the difference in test information functions (TIFs) between adjacent
#' next-stage modules to be within a specified tolerance:
#' \deqn{
#'   | I_{m_j}(\theta_r) - I_{m_{j+1}}(\theta_r) | \le \text{info\_tol}
#' }
#'
#' Adjacency is defined **by ordering of modules within the same stage**.
#' This ensures correct behavior even when some
#' pathways are excluded from the design.
#'
#' @param x An object of class \code{"mstATA_design"} created by \code{mst_design()}.
#'
#' @param rdp A numeric vector of routing decision points (theta values).
#' The length of \code{rdp} must equal the number of modules in
#' stage \code{which_stage + 1} minus one. Each element defines the routing
#' point between two adjacent modules in the next stage.
#'
#' @param which_stage A single integer indicating the stage *after which*
#' routing occurs. Routing constraints are applied to modules at
#' stage \code{which_stage + 1}. Routing cannot be defined after the final stage.
#'
#' @param info_tol A single positive numeric value specifying the maximum
#' allowable difference in information between adjacent modules at each
#' routing decision point.
#'
#' @details
#' The constraint enforces:
#'
#' \bold{The test information at the routing decision point between adjacent
#' modules in the next stage is similar within a specified tolerance.}
#'
#' Key characteristics:
#'
#' \itemize{
#'   \item The attribute type is \emph{quantitative}.
#'   \item The attribute is defined at the \emph{item level} in the item pool.
#'   \item The constraints are applied at the \bold{"Module-level"}.
#' }
#'
#' This function requires that item-level information functions
#' \code{iif(theta = theta_point)} already exist in the item pool.
#' These attributes can be prepared in advance using
#' \code{\link{compute_iif}}.
#'
#' The constraint is implemented as a pair of linear inequalities for
#' each routing decision point, resulting in two constraint rows per
#' routing decision point.
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
#' \eqn{m = 1, \ldots, M} denote the module index. Let
#' \eqn{x_{i_s m}} be a binary decision variable indicating whether item
#' \eqn{i_s} from stimulus \eqn{s} is selected into module \eqn{m}.
#'
#' Let \eqn{\theta_{rdp}} denote a routing decision point (RDP) at which examinees
#' are routed to different modules in the next stage of the test. The item
#' information function of item \eqn{i_s} at ability level \eqn{\theta_r} is
#' denoted by \eqn{I_{i_s}(\theta_{rdp})}, and the corresponding module-level test
#' information function is given by
#' \deqn{
#'   I_m(\theta_{rdp}) = \sum_{s=1}^{S} \sum_{i_s=1}^{I_s}
#'   I_{i_s}(\theta_{rdp}) \, x_{i_s m}.
#' }
#'
#' Consider routing after stage \eqn{k}. Let \eqn{\mathcal{M}_{k+1}} denote the
#' ordered set of modules at stage \eqn{k+1}. For each pair of adjacent modules
#' \eqn{(m_j, m_{j+1})} in \eqn{\mathcal{M}_{k+1}}, a routing decision point
#' \eqn{\theta_{r_j}} is specified. The routing information balance constraint
#' requires that the difference in test information between adjacent modules at
#' the routing decision point be bounded by a user-specified tolerance
#' \eqn{\delta > 0}:
#' \deqn{
#'   \left| I_{m_j}(\theta_{r_j}) - I_{m_{j+1}}(\theta_{r_j}) \right|
#'   \le \delta.
#' }
#'
#' Each absolute-value constraint is implemented as a pair of linear
#' inequalities, resulting in two constraint rows per routing decision point.
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
#' @seealso [compute_iif()]
#'
#' @examples
#' data("mini_itempool")
#' mini_itempool[,paste0("iif(theta=",c(-0.5,0.5),")")]<-compute_iif(mini_itempool,
#' item_par_cols = list("3PL"=c("discrimination","difficulty","guessing")),
#'                                                      theta = c(-0.5,0.5),model_col = "model")
#' test_mstATA <- mst_design(
#'   itempool = mini_itempool,module_length = c(3,4,4,4),
#'   design = "1-3"
#' )
#' test_rdp_con(x = test_mstATA,rdp = c(-0.5, 0.5),
#'              which_stage = 1,info_tol = 0.4)
#' @export




test_rdp_con<-function(x,rdp,
                       which_stage, info_tol = 0.5){
  if (!inherits(x, "mstATA_design")) {
    stop("Input 'x' must be an object of class 'mstATA_design'.")
  }
  ItemPool<-x$ItemPool
  PoolSize<-nrow(ItemPool)
  NumStages<-x$NumStages
  NumModules <- x$NumModules
  ModuleIndex<-x$ModuleIndex
  num_decisions<-PoolSize*NumModules
  if (length(which_stage) != 1L || !which_stage %in% seq_len(NumStages)) {
    stop(paste0("'which_stage' must be between 1 and ", NumStages), call. = FALSE)
  }
  if (which_stage == NumStages) {
    stop("Routing cannot be defined after the final stage.",call. = FALSE)
  }

  if (!is.numeric(info_tol) || length(info_tol) != 1L || info_tol <= 0) {
    stop("'info_tol' must be a single positive number.", call. = FALSE)
  }

  nextstage_modules <- ModuleIndex[ModuleIndex$stage == (which_stage + 1), "module_index"]
  nextstage_modules<-sort(nextstage_modules)
  num_nextstage_modules<-length(nextstage_modules)
  if(length(rdp)!=num_nextstage_modules-1){
    stop(paste0("Next stage has ",num_nextstage_modules," modules, require ",
                (num_nextstage_modules-1)," routing decision points."),
         call. = FALSE)
  }

  ## number of constraints
  n_rdp <- length(rdp)
  n_rows <- 2 * n_rdp
  ConstraintMatrix <- Matrix::Matrix(0,nrow = n_rows,ncol = num_decisions,sparse = TRUE)
  ConstraintMatrix_name <- character(n_rows)
  for (j in seq_len(n_rdp)) {
    theta_j <- rdp[j]
    attribute <- paste0("iif(theta=", theta_j, ")")
    item_vals <- get_attribute_val(ItemPool, attribute, cat_level = NULL)
    m_left  <- nextstage_modules[j]
    m_right <- nextstage_modules[j + 1]
    col_left  <- (m_left  - 1L) * PoolSize + seq_len(PoolSize)
    col_right <- (m_right - 1L) * PoolSize + seq_len(PoolSize)
    ## row indices
    r1 <- 2 * j - 1
    r2 <- 2 * j
    ConstraintMatrix[r1, col_left]  <-  item_vals
    ConstraintMatrix[r1, col_right] <- -item_vals
    ConstraintMatrix[r2, col_left]  <-  item_vals
    ConstraintMatrix[r2, col_right] <- -item_vals
    ConstraintMatrix_name[r1] <- paste0("RDP(", which_stage, "): Module", m_left, " - Module", m_right)
    ConstraintMatrix_name[r2] <- ConstraintMatrix_name[r1]
  }

  colnames(ConstraintMatrix)<-paste0("x[", rep(seq_len(NumModules), each = PoolSize), ",",
                                     rep(seq_len(PoolSize), NumModules), "]")
  decisionvar_name<-x$decisionvar_name
  if(length(decisionvar_name)!=num_decisions){
    ConstraintMatrix<-ConstraintMatrix[,decisionvar_name]
  }
  Specification <- data.frame(Requirement = "(MST structure) RDP",
                              Attribute = "IIF",
                              Type = "Quantitative",`Application Level` = "Module-level",
                              Operator = "(range)",`Num of Constraints` = n_rows,
                              stringsAsFactors = FALSE)
  colnames(Specification)<-c("Requirement","Attribute","Type","Application Level",
                             "Operator","Num of Constraints")
  Specification$`Num of Constraints`<-as.numeric(Specification$`Num of Constraints`)

  return(create_constraint(name = ConstraintMatrix_name,specification = Specification,
                           A_binary = ConstraintMatrix,A_real = NULL,
                           operators = rep(c(">=", "<="), n_rdp),
                           d = rep(c(-info_tol, info_tol), n_rdp),
                           C_binary = NULL, C_real = NULL))
}
