#' @title Concatenate Enemy Sets
#'
#' @description
#' Concatenates enemy sets produced by \code{create_enemy_sets()}.
#' Enemy sets from each source are **appended without modification**, while
#' pairwise enemy relationships are **unioned** across all inputs.
#' No transitive closure or set expansion is performed.
#'
#' @param ...
#' One or more enemy relationships returned by \code{create_enemy_sets()}.
#' Each input must be a list with the following components:
#' \describe{
#'   \item{ExclusionPair}{A two-column character matrix of pairwise enemy relationships.}
#'   \item{EnemySet}{A list of character vectors representing explicitly defined enemy sets.}
#' }
#'
#'
#' @details
#' Suppose source A defines five enemy sets and source B defines two enemy sets.
#' The resulting object will contain **seven enemy sets**, formed by simple
#' concatenation of the input \code{EnemySet} components.
#'
#' The \code{ExclusionPair} component of the result is the union of all unique,
#' unordered pairwise enemy relationships across inputs.
#'
#' Importantly, no transitive inference is applied. If item A is an enemy of B
#' and item B is an enemy of C (possibly from different sources), this function
#' does **not** infer that A is an enemy of C.
#'
#' This function is intended for situations where enemy relationships are
#' defined from multiple independent sources (e.g., similarity, cluing,
#' or security rules) and must be preserved exactly as specified.
#'
#' @return
#' A list with components:
#' \describe{
#'   \item{ExclusionPair}{A two-column character matrix of unioned enemy pairs.}
#'   \item{EnemySet}{A list of enemy sets formed by concatenating all inputs.}
#' }
#'
#' @seealso \code{\link{create_enemy_sets}}
#'
#' @examples
#' ## Example: combining multiple sources of enemy relationships
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
#' enemy_all
#' @export


concat_enemy_sets <- function(...) {
  sets <- list(...)

  if (length(sets) < 1L) {
    stop("At least one enemy_set must be provided.", call. = FALSE)
  }

  for (s in sets) validate_enemy_set(s)

  all_pairs <- do.call(rbind, lapply(sets, `[[`, "ExclusionPair"))
  all_enemy_sets <- unlist(lapply(sets, function(s) s$EnemySet), recursive = FALSE)

  if (is.null(all_pairs) || nrow(all_pairs) == 0L) {
    out <- list(
      ExclusionPair = matrix(ncol = 2, nrow = 0),
      EnemySet      = unlist(lapply(sets, `[[`, "EnemySet"), recursive = FALSE)
    )
    validate_enemy_set(out)
    return(out)
  }

  all_pairs <- as.matrix(all_pairs)
  all_pairs <- cbind(pmin(all_pairs[,1], all_pairs[,2]),
                     pmax(all_pairs[,1], all_pairs[,2]))
  all_pairs <- unique(all_pairs)

  out <- list(ExclusionPair = all_pairs,
              EnemySet      = all_enemy_sets)
  validate_enemy_set(out)

  return(out)
}


