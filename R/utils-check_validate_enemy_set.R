#' @title Validate Enemy Set Specification
#'
#' @description
#' Validates an enemy-set specification consisting of pairwise exclusions
#' (\code{ExclusionPair}) and an accompanying grouped representation
#' (\code{EnemySet}).
#'
#' @details
#' \code{ExclusionPair} is treated as the authoritative definition of enemy
#' relationships (pairwise exclusions). \code{EnemySet} is treated as an
#' explanatory or grouped representation of those exclusions.
#'
#' Validation enforces consistency and referential integrity between the two
#' components, but does not require \code{EnemySet} to enumerate all pairwise
#' exclusions implied by grouping (e.g., transitive closure or clique expansion).
#'
#' The following conditions are enforced:
#' \itemize{
#'   \item \code{ExclusionPair} must be a character matrix with exactly two columns.
#'   \item No self-pairs are allowed in \code{ExclusionPair}.
#'   \item No duplicated enemy pairs are allowed (order-insensitive).
#'   \item \code{EnemySet} must be a list of character vectors with length \eqn{\ge 2}.
#'   \item Every pair in \code{ExclusionPair} must appear together in at least one
#'         element of \code{EnemySet}.
#'   \item Every item appearing in \code{EnemySet} must appear in at least one row
#'         of \code{ExclusionPair}.
#' }
#'
#' @param enemy_set A list with components \code{ExclusionPair} and \code{EnemySet}.
#'
#' @return Invisibly returns \code{TRUE} if validation succeeds; otherwise an error
#'   is thrown.
#'
#' @keywords internal
#'
validate_enemy_set <- function(enemy_set) {
  if (!is.list(enemy_set)) {
    stop("`enemy_set` must be a list.", call. = FALSE)
  }

  required_names <- c("ExclusionPair", "EnemySet")
  missing_names <- setdiff(required_names, names(enemy_set))
  if (length(missing_names) > 0) {
    stop(paste0("`enemy_set` is missing required components: ",
                paste(missing_names, collapse = ", ")),call. = FALSE)
  }

  ExclusionPair <- enemy_set$ExclusionPair
  EnemySet      <- enemy_set$EnemySet

  if (!is.matrix(ExclusionPair) || !is.character(ExclusionPair)) {
    stop("`ExclusionPair` must be a character matrix.", call. = FALSE)
  }
  if (ncol(ExclusionPair) != 2) {
    stop("`ExclusionPair` must have exactly two columns.", call. = FALSE)
  }

  if (nrow(ExclusionPair) > 0L) {
    if (any(ExclusionPair[, 1] == ExclusionPair[, 2])) {
      stop("`ExclusionPair` contains self-pairs.", call. = FALSE)
    }
    ## canonicalize order for duplicate check
    sorted_pairs <- cbind(
      pmin(ExclusionPair[, 1], ExclusionPair[, 2]),
      pmax(ExclusionPair[, 1], ExclusionPair[, 2])
    )
    if (nrow(unique(sorted_pairs)) != nrow(sorted_pairs)) {
      stop("`ExclusionPair` contains duplicated enemy pairs.", call. = FALSE)
    }
  }


  if (!is.list(EnemySet)) {
    stop("`EnemySet` must be a list.", call. = FALSE)
  }
  if (length(EnemySet) > 0L) {
    if (!all(vapply(EnemySet, is.character, logical(1)))) {
      stop("Each element of `EnemySet` must be a character vector.",call. = FALSE)
    }

    if (any(vapply(EnemySet, length, integer(1)) < 2)) {
      stop("Each element of `EnemySet` must contain at least two elements.",call. = FALSE)
    }
  }

  ## ---- coverage check ----
  ## Every ExclusionPair must appear in at least one EnemySet
  if (nrow(ExclusionPair) > 0L) {
    pair_in_any_set <- function(i, j) {
      any(vapply(EnemySet,function(s) i %in% s && j %in% s,logical(1)))
    }

    missing_pairs <- !apply(ExclusionPair,1,function(p) pair_in_any_set(p[1], p[2]))

    if (any(missing_pairs)) {
      stop("Some `ExclusionPair` rows are not represented in any `EnemySet`.",
           call. = FALSE)
    }
  }
  ## every item in EnemySet must participate in at least one declared exclusion.
  items_in_pairs <- unique(c(ExclusionPair))
  items_in_sets  <- unique(unlist(EnemySet))
  orphan_items <- setdiff(items_in_sets, items_in_pairs)
  if (length(orphan_items) > 0) {
    stop("EnemySet contains items not appearing in ExclusionPair.")
  }

  invisible(TRUE)
}
