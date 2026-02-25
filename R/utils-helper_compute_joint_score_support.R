#' Compute Joint Score Support Under MST Routing
#'
#' Computes the set of possible joint (cumulative) number-correct scores
#' for examinees routed to each next-stage module in a multistage test (MST),
#' given the routing cut scores and the maximum score of the next module.
#'
#' This helper separates **score-support bookkeeping** from probability
#' computation. For each routing branch, it determines which joint scores
#' \eqn{S_{1:s} = S_{1:s-1} + S_s} are *theoretically attainable*, independent
#' of the ability level \eqn{\theta}. Unreachable score points are excluded
#' from the support and are later assigned probability zero.
#'
#' @param prev_scores Integer vector giving all possible cumulative scores
#'   at the previous stage \eqn{S_{1:s-1}} (typically \code{0:max_score}).
#'
#' @param next_mod_max_score Non-negative integer giving the maximum possible
#'   score on the next-stage module \eqn{S_s}. All integer scores from 0 to
#'   \code{next_mod_max_score} are attainable.
#'
#' @param lower Numeric vector of lower routing bounds. An examinee is routed
#'   to branch \eqn{j} if \code{prev_score > lower[j]}.
#'
#' @param upper Numeric vector of upper routing bounds. An examinee is routed
#'   to branch \eqn{j} if \code{prev_score <= upper[j]}.
#'
#' @details
#' For each routing branch \eqn{j}, the function:
#' \enumerate{
#'   \item Selects the subset of \code{prev_scores} satisfying the routing rule
#'         \eqn{lower_j < S_{1:s-1} \le upper_j}.
#'   \item Forms all possible joint scores by adding
#'         \eqn{S_s \in \{0, \dots, \text{next\_mod\_max\_score}\}}.
#'   \item Returns the sorted, unique joint score support for that branch.
#' }
#'
#' If no previous scores satisfy a routing branch, the corresponding support
#' is returned as an empty integer vector.
#'
#' @return
#' A list of integer vectors. Each element corresponds to one routing branch
#' and contains the possible joint score values for examinees routed to that
#' branch.
#'
#'
#' @keywords internal
compute_joint_score_support <- function(prev_scores,
                                        next_mod_max_score,
                                        lower,
                                        upper) {

  if (!is.numeric(prev_scores) || any(prev_scores < 0)) {
    stop("`prev_scores` must be a non-negative numeric vector.", call. = FALSE)
  }
  if (!is.numeric(next_mod_max_score) ||length(next_mod_max_score) != 1L ||next_mod_max_score < 0) {
    stop("`next_mod_max_score` must be a single non-negative integer.",
         call. = FALSE)
  }
  if (length(lower) != length(upper)) {
    stop("`lower` and `upper` must have the same length.", call. = FALSE)
  }

  out <- vector("list", length(lower))

  for (j in seq_along(lower)) {
    routed_prev <- prev_scores[prev_scores > lower[j] & prev_scores <= upper[j]]

    if (length(routed_prev) == 0L) {
      out[[j]] <- integer(0)
      next
    }

    joint_scores <- as.vector(outer(routed_prev, 0:next_mod_max_score, `+`))
    out[[j]] <- sort(unique(joint_scores))
  }

  out
}
