#' Discrete convolution of score distributions
#'
#' Computes the probability distribution of the sum of two independent
#' integer-valued score variables by discrete convolution.
#'
#' @details
#' Let \eqn{X} and \eqn{Y} be independent random variables representing
#' scores with supports \eqn{\{0,\dots,m\}} and \eqn{\{0,\dots,n\}},
#' respectively. Suppose their probability mass functions are
#'
#' \deqn{
#' \Pr(X = k) = p_k,\quad k = 0,\dots,m
#' }
#'
#' and
#'
#' \deqn{
#' \Pr(Y = \ell) = q_\ell,\quad \ell = 0,\dots,n.
#' }
#'
#' The probability mass function of the sum \eqn{S = X + Y} is given by
#' the discrete convolution
#'
#' \deqn{
#' \Pr(S = s) = \sum_{k=0}^{s} p_k\,q_{s-k},
#' \quad s = 0,\dots,m+n.
#' }
#'
#' This function evaluates the above convolution numerically using
#' \code{\link[stats]{convolve}}. Since \code{convolve()} in base R
#' computes cross-correlation by default, the second argument is
#' reversed internally to obtain the true convolution.
#'
#' In multistage testing (MST) and item response theory (IRT) applications,
#' this operation is used to combine item- or module-level score
#' distributions into a distribution for their total score at a given
#' ability level \eqn{\theta}.
#'
#' @param p A numeric vector of probabilities \eqn{(p_0,\dots,p_m)}
#'   representing the distribution of a score variable \eqn{X}.
#'   The \eqn{k}-th element corresponds to \eqn{\Pr(X = k)}.
#'
#' @param q A numeric vector of probabilities \eqn{(q_0,\dots,q_n)}
#'   representing the distribution of a score variable \eqn{Y}.
#'   The \eqn{\ell}-th element corresponds to \eqn{\Pr(Y = \ell)}.
#'
#' @return
#' A numeric vector of length \eqn{m + n + 1} giving the probability
#' distribution of the sum \eqn{S = X + Y}. The \eqn{s}-th element
#' corresponds to \eqn{\Pr(S = s)} for \eqn{s = 0,\dots,m+n}.
#'
#' @seealso
#' \code{\link[stats]{convolve}}
#'
#' @keywords internal

convolve_probs <- function(p, q) {
  # p and q are numeric probability vectors for scores 0..max
  as.numeric(stats::convolve(p, rev(q), type = "open"))
}
