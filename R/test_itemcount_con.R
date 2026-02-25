#' @title Generate Module- or Pathway-Level Constraints for the Number of Selected Items
#'
#' @description
#' This function generates linear constraints controlling the number of selected
#' items in each module or pathway of a MST panel.
#'
#' Depending on the MST design provided in \code{mst_design()}, the constraints
#' enforce either:
#'
#' \itemize{
#'   \item \strong{Module-level item counts} – each module is treated as an independent test form
#'   \item \strong{Pathway-level item counts} – each pathway is treated as an independent test form
#' }
#'
#'
#' The total number of constraints depends on the MST design and whether
#' stage-level bounds are used:
#'
#' \itemize{
#'   \item \strong{Module-level constraints:}
#'   The number of constraints is
#'     \strong{(number of modules)}
#'
#'   \item \strong{Pathway-level constraints:}
#'   The number of constraints is
#'     \strong{(number of pathways) + (number of modules with a min bound) + (number of modules with a max bound)
#'     + (number of equality constraints enforcing equal module lengths within each stage)}
#' }
#'
#' @param x An object of class \code{"mstATA_design"} created by \code{mst_design()}.
#' @param stage_length_bound Optional \code{data.frame} with columns:
#'   \code{stage}, \code{min}, \code{max}, specifying minimum and maximum
#'   allowable numbers of items in each stage.
#'   If \code{NULL}, the default rule is:
#'   **at least one item per stage** (no empty modules), and modules within each stage must have equal lengths.
#'
#'
#' @details
#' **1. Specification**
#'
#' The constraint enforces:
#'
#' \strong{the test must include the exact number of *items*.}.
#'
#' Key characteristics:
#'
#' \itemize{
#'   \item The attribute type is \emph{categorical}: each item has a unique ID.
#'   \item The attribute is defined at \emph{item level} in the item pool.
#'   \item The constraint can be enforced at either \strong{"Module-level"} or \strong{"Pathway-level"}.
#'   It depends on whether \code{module_length} or
#'         \code{pathway_length} was specified in \code{mst_design()}.
#' }
#'
#'
#' **2. Module-level constraints**
#'
#' These are generated when \code{module_length} was provided in \code{mst_design()}.
#'
#' \itemize{
#'   \item The required number of items per module is given by
#'         \code{x$ModuleIndex$ModuleLength}.
#'   \item One constraint is created per module.
#'   \item Each module must contain \emph{exactly} the specified number of items.
#' }
#'
#'
#' **3. Pathway-level constraints**
#'
#' These are generated when \code{pathway_length} was provided in \code{mst_design()}.
#'
#' \itemize{
#'   \item The required number of items per pathway is given by
#'         \code{x$PathwayIndex$PathwayLength}.
#'
#'   \item Optional stage-level constraints may enforce:
#'
#'    - Minimum items per stage
#'
#'    - Maximum items per stage
#'
#'   \item If \code{stage_length_bound = NULL}
#'
#'    - Each stage must contain at least one item
#'
#'    - All modules within a stage must have equal length
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
#' **1. Module-level constraint**
#'
#' \deqn{\sum_{s=1}^{S} \sum_{i_s=1}^{I_s} x_{i_s,m} = n_m}
#'
#' **2. Pathway-level constraint**
#'
#' \deqn{\sum_{m \in r} \sum_{s=1}^{S} \sum_{i_s=1}^{I_s} x_{i_s,m} = n_r}
#'
#' Stage-level minimum:
#'
#' \deqn{
#'   \sum_{s=1}^{S} \sum_{i_s=1}^{I_s} x_{i_s,m} \ge n_t^{\min}, \forall m \in \text{stage t}
#' }
#'
#' Stage-level maximum:
#'
#' \deqn{
#'   \sum_{s=1}^{S} \sum_{i_s=1}^{I_s} x_{i_s,m} \le n_t^{\max}, \forall m \in \text{stage t}
#' }
#'
#' Equal-length constraint for modules (m, m') in the same stage:
#'
#' \deqn{
#'   \sum_{s=1}^{S} \sum_{i_s=1}^{I_s} x_{i_s,m} = \sum_{s=1}^{S} \sum_{i_s=1}^{I_s} x_{i_s,m'}
#' }
#' Here:
#' \itemize{
#'   \item \eqn{x_{i_s,m}} is the binary decision variable indicating whether
#'   item \eqn{i_s} is selected into module m.
#'
#'   \item \eqn{m \in r} denote modules belonging to pathway r.
#'
#'   \item \eqn{n_m} specify the required number of items in module m.
#'
#'   \item \eqn{n_r} specify the required number of items in pathway r.
#'
#'   \item \eqn{n_t^{\min}} specify the minimum number of items in stage t.
#'
#'   \item \eqn{n_t^{\max}} specify the maximum number of items in stage t.
#'
#'   \item \eqn{m}, and \eqn{m'} are a pair of modules in stage t.
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
#' @examples
#' data("mini_itempool")
#' # Example 1: module-level test length
#' # Routing module: 3 items; Stage 2 modules: 4 items each
#' test_mstATA <- mst_design(
#'   itempool = mini_itempool,module_length = c(3,4,4,4),
#'   design = "1-3"
#' )
#' test_itemcount_con(test_mstATA)
#'
#' # Example 2: pathway-level test length: each pathway contains 7 items,
#' test_mstATA <- mst_design(
#'   itempool = mini_itempool,pathway_length = 7,
#'   design = "1-3"
#' )
#' # It will also constrain that each module has at least 1 item and modules
#' # in the stage 2 have the same module length.
#' test_itemcount_con(test_mstATA)
#'
#' # Example 3: pathway-level with stage-level max bound for Stage 1
#' test_itemcount_con(
#'   test_mstATA,
#'   stage_length_bound = data.frame(stage = 1, min = 1, max = 2)
#' )
#' @seealso [mst_structure_con()]
#' @export


test_itemcount_con <- function(x,stage_length_bound=NULL) {
  if (!inherits(x, "mstATA_design")) {
    stop("Input 'x' must be an object of class 'mstATA_design'.")
  }
  ModuleIndex<-validate_stage_length_bounds(x = x,stage_length_bound = stage_length_bound)
  ItemPool<-x$ItemPool
  PoolSize <- nrow(ItemPool)
  PathwayIndex <- x$PathwayIndex
  NumStages<-x$NumStages
  NumModules <- x$NumModules
  NumPathways <- x$NumPathways
  num_decisions<-PoolSize*NumModules

  i_idx<-integer()
  j_idx<-integer()
  ConstraintMatrix_name<-character()

  if ("ModuleLength" %in% names(ModuleIndex)) {
    message("'ModuleLength' present in design; ignoring stage-level bounds for length.")
    i_idx <- rep(seq_len(NumModules), each  = PoolSize)
    start<-1+PoolSize*(seq_len(NumModules)-1L)
    end<-PoolSize*seq_len(NumModules)
    j_idx <- as.vector(mapply(seq,start,end))
    ConstraintMatrix<-Matrix::sparseMatrix(i = i_idx,j = j_idx,x = 1L,dims = c(NumModules,num_decisions))
    ConstraintMatrix_name<-c(ConstraintMatrix_name,
                             paste("The number of items in Module",1:NumModules))
    operators<-rep("=",NumModules)
    rhs<-as.numeric(ModuleIndex$ModuleLength)
    Specification<-data.frame(a="(MST structure) Number of items in each module.",b="Item_id",c="Categorical",d="Module-level",
                              e="(exact)",f=NumModules,
                              stringsAsFactors = FALSE)
  }else{
    i_idx<-c(i_idx,rep(seq_len(NumPathways),each=PoolSize*NumStages))
    for (pathway_id in 1:NumPathways) {
      module_ids <- as.integer(unlist(PathwayIndex[pathway_id, 1:NumStages]))
      col_offset <- PoolSize*(module_ids-1L)
      j_idx<-c(j_idx,(rep(seq_len(PoolSize),NumStages)+rep(col_offset,each = PoolSize)))
    }
    operators<-rep("=",NumPathways)
    rhs<-PathwayIndex$PathwayLength
    ConstraintMatrix_name<-c(ConstraintMatrix_name,
                             paste("The number of items in Pathway",1:NumPathways))

    min<-ModuleIndex$min
    col_offset<-PoolSize*(seq_len(NumModules)-1L)
    i_idx<-c(i_idx,rep((seq_len(NumModules)+NumPathways),each = PoolSize))
    j_idx<-c(j_idx,(rep(seq_len(PoolSize),NumModules)+rep(col_offset,each=PoolSize)))
    operators<-c(operators,rep(">=",NumModules))
    rhs<-c(rhs,min)
    ConstraintMatrix_name<-c(ConstraintMatrix_name,
                             paste("The min number of items in Module",1:NumModules))
    col1<-c("Pathway item count",
            "Module item count(min)")
    col4<-c("Pathway-level","Module-level")
    col5<-c("(exact)","(min)")
    col6<-c(NumPathways,NumModules)

    if(any(!is.na(ModuleIndex$max))){
      which_module<-which(!is.na(ModuleIndex$max))
      max<-ModuleIndex$max[which_module]
      col_offset<-PoolSize*(which_module-1L)
      i_idx<-c(i_idx,rep((seq_along(which_module)+NumModules+NumPathways),each=PoolSize))
      j_idx<-c(j_idx,(rep(seq_len(PoolSize),length(which_module))+rep(col_offset,each=PoolSize)))
      operators<-c(operators,rep("<=",length(which_module)))
      rhs<-c(rhs,max)
      ConstraintMatrix_name<-c(ConstraintMatrix_name,
                               paste("The max number of items in Module",which_module))
      col1<-c(col1,paste0("Module item count (max)-module ",paste(which_module,collapse = ","),"."))
      col4<-c(col4,"Module-level")
      col5<-c(col5,"(max)")
      col6<-c(col6,length(which_module))
    }

    num_constraints<-length(ConstraintMatrix_name)
    ConstraintMatrix <- Matrix::sparseMatrix(i = i_idx,j = j_idx,x = 1L,
                                             dims = c(num_constraints,num_decisions))
    new_i_idx<-integer()
    new_j_idx<-integer()
    new_x<-integer()
    num_new<-0
    for(stage_id in 2:NumStages){
      module_instage<-which(x$ModuleIndex$stage==stage_id)
      num_new<-num_new+length(module_instage)-1
      temp_anc<-seq_len(PoolSize)+PoolSize*(module_instage[1]-1L)
      for(m in module_instage[2]:max(module_instage)){
        col_offset<-PoolSize*(m-1L)
        new_j_idx<-c(new_j_idx,c(temp_anc,(seq_len(PoolSize)+col_offset)))
        new_x<-c(new_x,rep(1L,PoolSize),rep(-1L,PoolSize))
        ConstraintMatrix_name<-c(ConstraintMatrix_name,paste0("Module ",module_instage[1]," and Module ",m," have equal length."))
      }
    }
    new_i_idx<-c(new_i_idx,rep(seq_len(num_new),each=PoolSize*2))
    new<-Matrix::sparseMatrix(i = new_i_idx,j = new_j_idx,x = new_x,
                              dims = c(num_new,num_decisions))
    ConstraintMatrix<-rbind(ConstraintMatrix,new)
    operators<-c(operators,rep("=",num_new))
    rhs<-c(rhs,rep(0,num_new))
    col1<-c(col1,"Same-length modules per stage")
    col4<-c(col4,"Module-level")
    col5<-c(col5,"(exact)")
    col6<-c(col6,num_new)
    temp_Specification<-data.frame(a=col1,b=rep("Item_id",length(col1)),c=rep("Categorical",length(col1)),
                                   d=col4,e=col5,f=col6,
                                   stringsAsFactors = FALSE)
    Specification<-data.frame(a = "(MST structure) Number of items in each pathway and module min/max.",
                              b = "Item_id",c = "Categorical", d = paste(unique(temp_Specification$d),collapse = ","),
                              e = paste(unique(temp_Specification$e),collapse = ","),
                              f = sum(temp_Specification$f))
  }

  colnames(Specification)<-c("Requirement","Attribute","Type","Application Level","Operator","Num of Constraints")
  Specification$`Num of Constraints`<-as.numeric(Specification$`Num of Constraints`)

  colnames(ConstraintMatrix)<-paste0("x[", rep(seq_len(NumModules), each = PoolSize), ",", rep(seq_len(PoolSize), NumModules), "]")
  decisionvar_name<-x$decisionvar_name
  if(length(decisionvar_name)!=num_decisions){
    ConstraintMatrix<-ConstraintMatrix[,decisionvar_name]
  }
  return(create_constraint(name=ConstraintMatrix_name,
                           specification = Specification,
                           A_binary = ConstraintMatrix,A_real = NULL,
                           operators = operators,d = rhs,
                           C_binary = NULL,C_real = NULL))
}

