#' Compute one-step joint score distribution under routing
#'
#' @description
#' Computes the joint (cumulative) score distribution after administering a
#' next-stage module, conditional on routing into a branch defined by a set of
#' reachable previous scores.
#'
#' This function performs a *single-step convolution* between:
#' \itemize{
#'   \item the conditional score distribution from previous stages, and
#'   \item the score distribution of the next module.
#' }
#'
#' No recursion is performed.
#'
#' The returned distribution is **not normalized**: for each theta column,
#' the column sum equals the probability of routing into this branch at that
#' theta.
#'
#' @param cdist_by_prev Numeric matrix.
#'   Rows correspond to previous score values given in \code{prev_scores};
#'   columns correspond to theta points.
#'
#' @param prev_scores Integer vector of non-negative values giving the score
#'   values associated with the rows of \code{cdist_by_prev}.
#'
#' @param icc_by_next_mod Named list of ICC matrices for the next module.
#'   Each element corresponds to a theta point and must be compatible with
#'   \code{\link{module_score_dist}}.
#'   Names must match the column names of \code{cdist_by_prev}.
#'
#' @param possible_joint_score Integer vector of non-negative cumulative scores
#'   that are reachable under this routing branch.
#'
#' @return
#' A numeric matrix with:
#' \itemize{
#'   \item rows corresponding exactly to \code{possible_joint_score},
#'   \item columns corresponding to theta points.
#' }
#'
#' Each column represents the (unnormalized) joint score distribution at that
#' theta.
#'
#' @details
#' Let \eqn{S_{prev}} denote the cumulative score prior to the current stage,
#' and \eqn{S_{next}} the score from the next-stage module.
#' This function computes:
#'
#' \deqn{
#'   P(S = s \mid \theta)
#'   = \sum_{k} P(S_{prev} = k \mid \theta)
#'     \, P(S_{next} = s - k \mid \theta)
#' }
#'
#' for all s in \code{possible_joint_score}.
#'
#' @examples
#' ## toy previous score distribution
#' prev_scores <- 0:2
#' cdist_by_prev <- matrix(
#'   c(0.2, 0.3,
#'     0.5, 0.4,
#'     0.3, 0.3),
#'   nrow = 3,
#'   dimnames = list(NULL, c("theta=-1", "theta=0"))
#' )
#'
#' ## simple ICC: two items, dichotomous
#' icc <- matrix(
#'   c(0.4, 0.6,
#'     0.7, 0.3),
#'   nrow = 2,
#'   byrow = TRUE
#' )
#' colnames(icc) <- c("0", "1")
#'
#' icc_by_next_mod <- list(
#'   "theta=-1" = icc,
#'   "theta=0"  = icc
#' )
#'
#' possible_joint_score <- c(1, 2, 3)
#'
#' joint_module_score_dist(
#'   cdist_by_prev,
#'   prev_scores,
#'   icc_by_next_mod,
#'   possible_joint_score
#' )
#'
#' @export
joint_module_score_dist <- function(cdist_by_prev,
                                    prev_scores,
                                    icc_by_next_mod,
                                    possible_joint_score) {
  ## ---- validation ----
  if (!is.matrix(cdist_by_prev) || !is.numeric(cdist_by_prev) || anyNA(cdist_by_prev)) {
    stop("`cdist_by_prev` must be a numeric matrix with no NA.", call. = FALSE)
  }

  if (!is.numeric(prev_scores) || anyNA(prev_scores) ||
      any(prev_scores %% 1 != 0) || any(prev_scores < 0)) {
    stop("`prev_scores` must be a vector of non-negative integers.", call. = FALSE)
  }

  if (nrow(cdist_by_prev) != length(prev_scores)) {
    stop("`nrow(cdist_by_prev)` must equal `length(prev_scores)`.", call. = FALSE)
  }

  if (!is.list(icc_by_next_mod) || length(icc_by_next_mod) == 0) {
    stop("`icc_by_next_mod` must be a non-empty list of ICC matrices.", call. = FALSE)
  }

  if (!is.numeric(possible_joint_score) || anyNA(possible_joint_score) ||
      any(possible_joint_score %% 1 != 0) || any(possible_joint_score < 0)) {
    stop("`possible_joint_score` must be a vector of non-negative integers.", call. = FALSE)
  }

  theta_names <- colnames(cdist_by_prev)
  if (is.null(theta_names)) {
    stop("`cdist_by_prev` must have column names (theta grid).", call. = FALSE)
  }

  if (is.null(names(icc_by_next_mod))) {
    stop("`icc_by_next_mod` must be a *named* list with theta names.", call. = FALSE)
  }

  if (!setequal(theta_names, names(icc_by_next_mod))) {
    stop("Theta grid mismatch between `cdist_by_prev` and `icc_by_next_mod`.",
         call. = FALSE)
  }

  ## ensure consistent order
  icc_by_next_mod <- icc_by_next_mod[theta_names]
  ## ---- compute next-module score distribution ----
  cdist_by_next <- module_score_dist(icc_by_next_mod)

  if (!identical(colnames(cdist_by_next), theta_names)) {
    stop("Theta grid mismatch in `module_score_dist()` output.", call. = FALSE)
  }
  next_scores <- seq_len(nrow(cdist_by_next)) - 1L

  ## prepare output container: rows correspond EXACTLY to possible_joint_score
  possible_joint_score <- sort(unique(as.integer(possible_joint_score)))
  score_index <- as.character(possible_joint_score)
  out <- matrix(0,
                nrow = length(score_index),
                ncol = length(theta_names),
                dimnames = list(score_index, theta_names))

  ## ---- recursion ----
  for (i in seq_along(prev_scores)) {
    ps <- prev_scores[i]
    p_prev <- cdist_by_prev[i, ]          # vector over theta

    for (j in seq_along(next_scores)) {
      ns <- next_scores[j]
      joint_score <- ps + ns
      key<-as.character(joint_score)

      if (key %in% score_index) {
        out[key, ] <-out[key, ] +p_prev * cdist_by_next[j, ]
      }
    }
  }

  out
}
