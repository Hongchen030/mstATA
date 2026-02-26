#' @title Create an mstATA_design Object
#'
#' @description
#' Constructs an \code{mstATA_design} object from an item pool and a multistage test (MST)
#' design. The resulting object encodes the full structural
#' information required for automated test assembly, including module structure,
#' pathway structure, decision variables, and optional **preprocessing** artifacts
#' such as stimulus mappings and enemy relationships.
#'
#' @param itempool A \code{data.frame} containing the item pool. Each row represents
#'   one item and may include metadata such as item ID, difficulty, content category,
#'   or stimulus membership.
#' @param item_id_col A character string giving the column name in \code{itempool}
#'   that uniquely identifies items.
#' @param design A character string specifying the MST design. The string encodes
#'   the number of modules per stage and may use commas (\code{,}), dashes
#'   (\code{-}), or slashes (\code{/}) as separators.
#'   For example, \code{"1-3-3"}, \code{"1,3,3"}, and \code{"1/3/3"} all define
#'   an MST with 1 module in stage 1, 3 modules in stage 2, and 3 modules in stage 3.
#' @param rdp Optional list specifying routing decision points for each
#' stage transition in a multistage test. Default is NULL. See Details.
#' @param diff_levels Optional named list of difficulty labels, indexed by the number
#'   of modules in each stage. If \code{NULL}, default labels are used for up to
#'   five modules per stage. If any stage has more than five modules and
#'   \code{diff_levels} is not provided, difficulty labels are omitted.
#' @param exclude_pathways Optional character vector specifying disallowed pathways.
#'   Each element must be a string of the form \code{"x-y-z"}, where each number
#'   refers to a module index at the corresponding stage. This is commonly used to
#'   prohibit extreme transitions (e.g., routing from an easy module directly to
#'   a hard module).
#' @param module_length Optional numeric vector giving the number of items in each
#'   module. The length must match the number of modules implied by \code{design}.
#' @param pathway_length Optional numeric scalar specifying the total number of items
#'   in each pathway. All pathways must have equal length if this argument is used.
#' @param item_module_eligibility Optional named list specifying item eligibility
#'   by module. List names correspond to (a subset of) module indices (within a single panel),
#'   and each element is a vector of item indices eligible for that module.
#'   If omitted, all items are eligible for that module.
#' @param pivot_stim_map Optional. A precomputed stimulus–item mapping created by
#'   \code{\link{create_pivot_stimulus_map}}. Required for stimulus-based constraints.
#' @param enemyitem_set Optional. A precomputed enemy item set created by
#'   \code{\link{create_enemy_sets}} (and optionally combined via
#'   \code{\link{concat_enemy_sets}}). Required for enemy-item constraints.
#' @param enemystim_set Optional. A precomputed enemy stimulus set created by
#'   \code{\link{create_enemy_sets}} (and optionally combined via
#'   \code{\link{concat_enemy_sets}}). Required for enemy-stimulus constraints.
#'
#' @return An object of class \code{"mstATA_design"} with the following components:
#' \describe{
#'   \item{ItemPool}{The original item pool.}
#'   \item{item_id_col}{A character string giving the column name in \code{ItemPool}
#'   that uniquely identifies items.}
#'   \item{pivot_stim_map}{Optional verified stimulus mapping object with components
#'     \code{pivot_item_id}, \code{stimulus_name},
#'     \code{stimulus_members}, and \code{numItems_stimulus}.}
#'   \item{enemyitem_set}{Optional verified enemy item set containing
#'     \code{ExclusionPair}, \code{EnemySet}, and derived \code{ItemIndex}.}
#'   \item{enemystim_set}{Optional verified enemy stimulus set containing
#'     \code{ExclusionPair}, \code{EnemySet}, and derived \code{PivotIndex}.}
#'   \item{NumStages}{Number of stages in the MST design.}
#'   \item{NumModules}{Number of modules in the MST design.}
#'   \item{NumPathways}{Number of allowed pathways.}
#'   \item{RDP}{Optional verified routing decision points.}
#'   \item{ModuleIndex}{A \code{data.frame} describing module structure by stage.
#'     If \code{module_length} is supplied, includes a \code{ModuleLength} column.}
#'   \item{PathwayIndex}{A \code{data.frame} describing allowed pathways and their
#'     total lengths. If difficulty labels are available, includes pathway labels.}
#'   \item{decisionvar_name}{Character vector of decision variable names
#'     (one panel).}
#'   \item{decisionvar_type}{Character vector of decision variable types
#'     (currently all binary).}
#' }
#'
#' @details
#' **1. Routing decision points**
#'
#' The list must have length \code{NumStages - 1}. Each element corresponds
#' to routing after a given stage and must be a numeric vector whose length
#' equals the number of modules in the next stage minus one.
#'
#' For example, in a three-stage 1–3–3 design:
#' \preformatted{
#' rdp = list(
#'   c(-0.5, 0.5),  # routing after stage 1
#'   c(-1, 1)       # routing after stage 2
#' )
#' }
#'
#' The ordering of values within each vector defines adjacency among
#' modules in the next stage.
#'
#' **2. module_length or pathway_length**
#'
#' Exactly one of \code{module_length} or \code{pathway_length} must be specified.
#'
#' \itemize{
#'   \item If \code{module_length} is provided, modules at the same stage are checked (must
#'   have equal length). Pathway lengths are computed automatically.
#'   \item If \code{pathway_length} is provided, all pathways are assumed to have
#'   equal length and module lengths are not explicitly stored.
#'   \item If both are provided, consistency between the implied and supplied
#'   pathway lengths is checked.
#' }
#'
#' **3. item_module_eligibility set**
#'
#' By default, all items are eligible for all modules, yielding a fully flexible
#' module-explicit formulation with decision variables
#' \eqn{x_{m,i,p}} indicating whether item \eqn{i} is selected in module \eqn{m}
#' of panel \eqn{p}. Item usage constraints ensure uniqueness within a pathway
#' or across a panel.
#'
#' When \code{item_module_eligibility} is supplied, decision variables are created
#' only for eligible item–module combinations. This reduces model size while
#' preserving flexibility near module boundaries and avoids rigid item partitioning.
#'
#'
#' @seealso
#' \code{\link{create_pathways}},
#' \code{\link{create_pivot_stimulus_map}},
#' \code{\link{create_enemy_sets}},
#' \code{\link{concat_enemy_sets}}
#'
#' @examples
#' # mini item pool
#' data("mini_itempool")
#' # Example 1: 1-3-3 design with varying module lengths
#' mst_design(itempool = mini_itempool,design = "1-3-3",
#'                module_length = c(6, 4, 4, 4, 3, 3, 3),pathway_length = NULL)
#'
#' # Example 2: 1-3-3 design with fixed pathway length and excluded REH and RHE pathways
#' mst_design(itempool = mini_itempool,design = "1-3-3",
#'                exclude_pathways = c("1-1-3", "1-3-1"),
#'                module_length = NULL,pathway_length = 13)
#' # Example 3: 1-3-3 design, with item_module_eligibility provided
#' item_module_eligibility<-list(`1`=1:nrow(mini_itempool),
#'                               `2`=which(mini_itempool$difficulty<=0),
#'                               `4`=which(mini_itempool$difficulty>=0))
#' ### selected items in routing module no item-level requirement,
#' ### selected items in S2E, difficulty below 0
#' ### selected items in S2H, difficulty above 0
#'
#' mst_design(itempool = mini_itempool,design = "1-3-3",
#'            module_length = c(6,4,4,4,3,3,3),
#'            item_module_eligibility = item_module_eligibility)
#'
#' @references
#' Xiong, X. (2018). A hybrid strategy to construct multistage adaptive tests.
#' \emph{Applied Psychological Measurement}, 42(8), 630–643.{doi:10.1177/0146621618762739}
#'
#' @export

mst_design <- function(itempool,item_id_col = "item_id",
                       design,rdp = NULL,diff_levels=NULL,exclude_pathways=NULL,
                       module_length = NULL, pathway_length = NULL,
                       item_module_eligibility = NULL,
                       pivot_stim_map = NULL,
                       enemyitem_set = NULL,
                       enemystim_set = NULL) {

  if (is.null(module_length) && is.null(pathway_length)) {
    stop("Either 'module_length' or 'pathway_length' must be provided.")
  }

  if (!is.data.frame(itempool)) stop("'itempool' must be a data frame.")

  item_ids <- as.character(check_attribute_column(itempool, item_id_col))
  if (anyDuplicated(item_ids)) {
    stop("Item identifiers must be unique.", call. = FALSE)
  }
  PoolSize <- nrow(itempool)
  index_map <- setNames(seq_len(PoolSize), item_ids)
  if (!is.null(pivot_stim_map)) {
    validate_pivot_stim_map(pivot_stim_map, itempool,item_id_col)
    pivot_idx <- index_map[pivot_stim_map$pivot_item_id]
    stim_member_idx <- lapply(
      pivot_stim_map$stimulus_members,
      function(ids) {
        unname(as.integer(index_map[ids]))
      }
    )

    pivot_stim_map$pivot_item_id    <- unname(as.integer(pivot_idx))
    pivot_stim_map$stimulus_members <- stim_member_idx
    validate_pivot_stim_index_map(pivot_stim_map,itempool)
  }

  if (!is.null(enemyitem_set)) {
    validate_enemy_set(enemyitem_set)

    enemyitem_set$ItemIndex <- lapply(
      enemyitem_set$EnemySet,
      function(ids) {
        idx <- index_map[ids]
        if (any(is.na(idx))) {
          stop("enemyitem_set contains item IDs not found in itempool.",call. = FALSE)
        }
        unname(as.integer(idx))
      }
    )
    validate_enemy_set_index(enemyitem_set,itempool)
  }

  if (!is.null(enemystim_set)) {
    if (is.null(pivot_stim_map)) {
      stop("enemystim_set requires pivot_stim_map to be provided.",call. = FALSE)
    }

    validate_enemy_set(enemystim_set)

    stim_to_pivot <- setNames(pivot_stim_map$pivot_item_id,pivot_stim_map$stimulus_name)
    enemystim_set$ItemIndex <- lapply(
      enemystim_set$EnemySet,
      function(stims) {
        piv <- stim_to_pivot[stims]
        if (any(is.na(piv))) {
          stop("enemystim_set contains stimulus IDs not found in pivot_stim_map.",call. = FALSE)
        }
        unname(as.integer(piv))
      }
    )
    validate_enemy_set_index(enemystim_set,itempool)
  }

  Structure <- create_pathways(design = design, diff_levels = diff_levels,exclude_pathways = exclude_pathways)
  ModuleIndex <- Structure$Modules
  PathwayIndex <- Structure$Pathways
  NumStages<-length(unique(ModuleIndex$stage))
  NumModules<-nrow(ModuleIndex)

  item_module_eligibility<-check_item_module_eligibility(item_module_eligibility,index_map = index_map,
                                                         NumModules = NumModules)
  PathwayIndex<-PathwayIndex[PathwayIndex$allowed,]
  PathwayIndex$pathway_index <- seq_len(nrow(PathwayIndex))
  rownames(PathwayIndex) <- NULL
  NumPathways<-nrow(PathwayIndex)

  if (!is.null(module_length)) {
    if (length(module_length) != NumModules) {
      stop("Length of 'module_length' must equal the number of modules (", NumModules, ").")
    }
    ModuleIndex$ModuleLength <- module_length

    stage_length <- numeric()
    for (stage_id in 1:NumStages) {
      lengths <- ModuleIndex$ModuleLength[ModuleIndex$stage == stage_id]
      if (length(unique(lengths)) != 1) {
        stop("All modules in stage ", stage_id, " must have the same length. Found: ", toString(unique(lengths)))
      }
      stage_length <- c(stage_length, unique(lengths))
    }
    cal_pathway_length <- sum(stage_length)
    PathwayIndex$PathwayLength <- cal_pathway_length
    if(!is.null(pathway_length) && unique(pathway_length)!=cal_pathway_length){
        stop("The provided `pathway_length` is not equal to the length calculated by the sum of stagelength.")
    }
  }


  if (!is.null(pathway_length)) {
    if (length(unique(pathway_length)) != 1) {
      stop("All pathway lengths must be equal. Got: ", toString(unique(pathway_length)))
    }
    PathwayIndex$PathwayLength <- pathway_length
  }
  rdp<-check_rdp(rdp = rdp,ModuleIndex = ModuleIndex,NumStages = NumStages)
  decisionvar_name <- c()
  for (module_id in seq_len(NumModules)) {
    candidate <- item_module_eligibility[[as.character(module_id)]]
    decisionvar_name <- c(decisionvar_name,
                          paste0("x[", module_id, ",", candidate, "]"))
  }

  result <- list(ItemPool = itempool,item_id_col = item_id_col,
                 pivot_stim_map = pivot_stim_map,
                 enemyitem_set = enemyitem_set,
                 enemystim_set = enemystim_set,
                 NumStages = NumStages,NumModules = NumModules,NumPathways = NumPathways,
                 RDP = rdp,
                 ModuleIndex = ModuleIndex,PathwayIndex = PathwayIndex,
                 decisionvar_name = decisionvar_name,
                 decisionvar_type = rep("B",length(decisionvar_name)))
  class(result) <- "mstATA_design"
  return(result)
}

