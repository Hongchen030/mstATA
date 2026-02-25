#' @title Create Enemy Pairs and Enemy Sets
#'
#' @description
#' Create pairwise enemy relationships and corresponding enemy sets for items
#' or stimuli based on metadata in the item pool.
#'
#' This function parses enemy information from two aligned vectors—
#' an identifier column and an enemy-specification column—and returns
#' (1) a matrix of unique enemy pairs and (2) a list of enemy sets.
#' Enemy pairs represent item or stimulus combinations that must not be
#' selected together. See Details.
#'
#'
#' The function is intended as a **preprocessing step** prior to constructing
#' enemy-item or enemy-stimulus exclusion constraints.
#'
#' @param id_col A character vector of item or stimulus identifiers
#'   (e.g., item IDs or stimulus names).
#' @param enemy_col A character vector specifying enemy items or stimuli
#'   associated with each element of \code{id_col}. The length must equal
#'   \code{length(id_col)}. Missing values indicate no enemies.
#' @param sep_pattern A regular expression used to split multiple enemy
#'   identifiers within \code{enemy_col}. Defaults to \code{","}.
#'
#' @details
#' **1. Enemy definition**
#'
#' Two items or two stimuli are considered \emph{enemies} if they must not appear
#' together. Typical reasons include:
#'
#' \itemize{
#'   \item High surface similarity or paraphrasing
#'   \item Shared clues or solution pathways
#'   \item Security, exposure, or operational constraints
#' }
#'
#' The \code{ExclusionPair} component is the \emph{authoritative definition}
#' of explicitly declared enemy relationships. Each row represents a single,
#' directly specified pairwise exclusion.
#'
#' The \code{EnemySet} component provides a grouped representation of enemy
#' relationships for constraint construction. Enemy sets may include items
#' or stimuli that are connected through one or more pairwise exclusions in
#' \code{ExclusionPair}. Formally, each enemy set corresponds to a connected
#' component of the graph induced by \code{ExclusionPair}.
#'
#'
#' **2. Interpretation of enemy relationships**
#'
#' Enemy relationships specified through \code{enemy_col} are interpreted
#' \emph{transitively}. That is, if item/stimulus A is an enemy of B, and
#' B is an enemy of C, then A, B, and C are merged into a single
#' \emph{enemy set}. From each enemy set, at most one item or stimulus
#' may be selected.
#'
#' This conservative interpretation prevents indirect information leakage
#' or similarity chains that may not be obvious from pairwise specifications
#' alone.
#'
#' **3. Important modeling note**
#'
#' If users intend different \emph{types} of enemy relationships that should
#' \emph{not} propagate transitively, they should encode them in
#' \emph{separate columns} of the item pool.
#'
#' For example:
#' \itemize{
#'   \item Item A is an enemy of item B because they are \emph{too similar}.
#'   \item Item B is an enemy of item C because they \emph{provide clues to each other}.
#'   \item The user does \emph{not} believe that item A and item C should be treated
#'         as enemies.
#' }
#'
#' In this case, a safer modeling strategy is:
#' \itemize{
#'   \item Encode the A–B enemy relationship in one enemy column
#'         (e.g., \code{enemy_similarity});
#'   \item Encode the B–C enemy relationship in a different enemy column
#'         (e.g., \code{enemy_clueing});
#'   \item Construct enemy sets separately from each column.
#' }
#'
#' This approach prevents unintended transitive grouping, allowing items A and C
#' to remain eligible for joint selection while still enforcing the intended
#' pairwise exclusions.
#'
#' In this case, users construct enemy sets separately, concatenate them using concat_enemy_sets(),
#' and supply the concatenated result to mst_design().
#'
#' @return A list with the following components:
#' \describe{
#'   \item{ExclusionPair}{A two-column character matrix in which each row
#'     represents a unique pair of mutually exclusive (enemy) items or stimuli.}
#'   \item{EnemySet}{A list of character vectors, where each element represents
#'     a set of items or stimuli that are mutually exclusive with one another.}
#' }
#'
#' @seealso [concat_enemy_sets()]
#'
#' @examples
#' ## Example 1: Enemy item relationships
#' create_enemy_sets(
#'   id_col = mini_itempool$item_id,
#'   enemy_col = mini_itempool$enemy_similarity,
#'   sep_pattern = ","
#' )
#'
#' ## Example 2: Enemy stimulus relationships
#' create_enemy_sets(
#'   id_col = reading_itempool$stimulus,
#'   enemy_col = reading_itempool$enemy_stimulus,
#'   sep_pattern = ", "
#' )
#' @export


create_enemy_sets <- function(id_col,enemy_col, sep_pattern = ",") {
  id_col<-as.character(id_col)
  enemy_col<-as.character(enemy_col)
  splitted_list <- strsplit(enemy_col, split = sep_pattern)
  splitted_list <-lapply(splitted_list,trimws)
  out_pairs <- do.call(rbind, lapply(seq_along(id_col), function(i) {
    id <- id_col[i]
    enemies <- splitted_list[[i]]
    enemies <- enemies[enemies != "" & !is.na(enemies)]  # clean
    if (length(enemies) == 0) return(NULL)
    cbind(id, enemies)
  }))
  if (is.null(out_pairs)||length(out_pairs)==0L) {
    return(list(ExclusionPair = matrix(ncol = 2, nrow = 0), EnemySet = list()))
  }
  out_pairs<-as.matrix(out_pairs)

  out_pairs <- cbind(pmin(out_pairs[,1], out_pairs[,2]),
                     pmax(out_pairs[,1], out_pairs[,2]))
  out_pairs <- unique(out_pairs)

  rownames(out_pairs) <- NULL
  if (any(out_pairs[, 1] == out_pairs[, 2])) {
    self_pairs <- out_pairs[out_pairs[, 1] == out_pairs[, 2], ]
    self_pairs <- as.matrix(self_pairs)
    stop("Some items/stimuli are paired with themselves: ", paste(unique(self_pairs[, 1]), collapse = ", "))
  }

  missing_ids <- setdiff(out_pairs, id_col)
  if (length(missing_ids) > 0) {
    stop("The following item/stimulus ids are in the exclusion list but not in the item pool: ",
            paste(missing_ids, collapse = ", "))
  }

  edges <- out_pairs
  nodes<-sort(unique(c(edges)))
  parent<-nodes
  names(parent)<-nodes
  find_root <- function(x) {
    if (parent[x] != x) parent[x] <<- find_root(parent[x])
    parent[x]
  }
  union_set <- function(a, b) {
    ra <- find_root(a); rb <- find_root(b)
    if (ra != rb) parent[rb] <<- ra
  }
  for (i in seq_len(nrow(edges))) {
    union_set(edges[i, 1], edges[i, 2])
  }
  reps <- character(length(nodes))
  names(reps) <- nodes
  for (i in seq_along(nodes)) {
    reps[i] <- find_root(nodes[i])
  }
  groups <- split(names(reps), reps)
  names(groups)<-NULL
  return(list(ExclusionPair = out_pairs,EnemySet = groups))

}

