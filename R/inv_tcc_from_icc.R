#' @title Inverse test characteristic curve from ICCs
#'
#' @description
#'
#' Numerically inverts a module test characteristic curve (TCC) to obtain the ability
#' corresponding to a given observed total score.
#'
#' @param icc_list A named list of item category probability
#'   matrices across a grid of ability values. Each element corresponds to
#'   one ability level and must be named \code{"theta=<value>"}. Rows
#'   represent items and columns represent response categories
#'   (\code{cat0}, \code{cat1}, \dots). Can be created by \code{compute_icc()}.
#'
#' @param target_score A numeric value giving the target expected total
#'   score for which the inverse TCC is to be computed.
#'
#' @param range_tcc A numeric vector of length two specifying the lower
#'   and upper bounds of the ability interval used for inversion.
#'   Defaults to \code{c(-5, 5)}.
#'
#' @param tol A numeric tolerance passed to the root-finding algorithm.
#'   Smaller values yield more precise solutions at increased
#'   computational cost. Default is 1e-4.
#'
#' @section Mathematical Formulation:
#'
#' Consider a module consisting of \eqn{I} items. Let the score for item
#' \eqn{i} be a discrete random variable \eqn{Y_i} taking values
#' \eqn{\{0, 1, \dots, m\}}, where \eqn{m} is the maximum item score
#' for item i. Conditional on ability \eqn{\theta}, item
#' responses are assumed to be locally independent, with category
#' probabilities
#'
#' \deqn{
#' \Pr(Y_i = k \mid \theta) = p_{ik}(\theta),
#' \quad k = 0,\dots,m.
#' }
#'
#' The expected score for item \eqn{i} at ability level \eqn{\theta} is
#'
#' \deqn{
#' E(Y_i \mid \theta) = \sum_{k=0}^{m} k \, p_{ik}(\theta),
#' }
#'
#' and the expected total module score (the test characteristic curve) is
#'
#' \deqn{
#' T(\theta) = E(S \mid \theta)
#'   = \sum_{i=1}^{I} E(Y_i \mid \theta),
#' }
#'
#' where \eqn{S = \sum_{i=1}^{I} Y_i} denotes the total module score.
#'
#' Given a target score \eqn{s^\ast}, the inverse TCC problem is to find
#' \eqn{\hat{\theta}} such that
#'
#' \deqn{
#' T(\hat{\theta}) = s^\ast.
#' }
#'
#' Because \eqn{T(\theta)} generally has no closed-form inverse, this
#' function evaluates \eqn{T(\theta)} on a grid of ability values using
#' ICCs and constructs a continuous approximation via linear
#' interpolation. A root-finding algorithm is then used to solve
#'
#' \deqn{
#' T(\theta) - s^\ast = 0
#' }
#'
#' on a specified ability interval.
#'
#' \strong{3PL and Non-Invertible Scores}
#'
#' When items follow the three-parameter logistic (3PL) model with
#' guessing parameters \eqn{g_i > 0}, the TCC has a nonzero lower bound:
#' \deqn{
#'   \lim_{\theta \to -\infty} \text{TCC}(\theta)
#'   = \sum_{i=1}^I g_i = G.
#' }
#'
#' Therefore, any NC score \eqn{S < G} cannot be obtained from the
#' TCC for any finite \eqn{\theta}, and the inverse TCC is undefined
#' in the strict sense.
#'
#' To ensure a monotone score-to-theta mapping, the minimum
#' invertible NC score is defined as
#' \deqn{
#'   X = \lceil G \rceil,
#' }
#' the smallest integer strictly greater than the guessing floor.
#'
#' Let \eqn{\theta_{\min}} denote the lower bound of the restricted
#' ability range (given by `range_tcc[1]`), and let \eqn{\theta_X}
#' denote the inverse-TCC solution corresponding to score \eqn{X}.
#'
#' For any score \eqn{Y} such that \eqn{0 \le Y < X}, the ability
#' estimate is obtained by linear interpolation between
#' \eqn{(0, \theta_{\min})} and \eqn{(X, \theta_X)}:
#'
#' \deqn{
#'   \theta^*(Y)
#'   = \theta_{\min}
#'     + \frac{Y}{X}
#'       \left(\theta_X - \theta_{\min}\right).
#' }
#'
#' This construction:
#' \itemize{
#'   \item Preserves monotonicity of the score-to-theta function;
#'   \item Respects the lower bound imposed by 3PL guessing;
#'   \item Ensures continuity at \eqn{Y = X};
#'   \item Avoids artificial extrapolation of the TCC.
#' }
#'
#' Scores above the maximum attainable TCC value are mapped to
#' the upper bound `range_tcc[2]`.
#'
#' In multistage testing (MST), inverse TCC values are used to equate number
#' correct scores across pathways and to evaluate routing accuracy, bias, and
#' conditional standard errors of measurement.
#'
#'
#' @return
#' A single numeric value giving the ability
#' such that the expected module score at this ability equals the
#' target score, up to numerical tolerance.
#'
#' @examples
#' # Example with two dichotomous items and two theta values
#' items<-data.frame(discrimination = c(1,1),difficulty = c(-1,0),model= rep("2PL",2))
#'
#' icc_list<-compute_icc(items = items,
#'                       item_par_cols = list("2PL"=c("discrimination","difficulty")),
#'                       theta = seq(-5,5,0.1),model_col = "model")
#'
#' inv_tcc_from_icc(icc_list, target_score = 1)
#'
#' # Example with two dichotomous items, (guessing is allowed)
#' items<-data.frame(discrimination = c(1,1),difficulty = c(-1,0),guessing = c(0.2, 0.2),
#'                   model= rep("3PL",2))
#' icc_list<-compute_icc(items = items,
#'                       item_par_cols = list("3PL"=c("discrimination","difficulty","guessing")),
#'                       theta = seq(-5,5,0.1),
#'                       model_col = "model")
#' inv_tcc_from_icc(icc_list, target_score = 0)
#'
#' @export

inv_tcc_from_icc <- function(icc_list, target_score,
                             range_tcc = c(-5, 5),
                             tol = 1e-4) {
  if (!is.list(icc_list) || length(icc_list) == 0) {
    stop("`icc_list` must be a non-empty list.", call. = FALSE)
  }
  if (!is.numeric(target_score) || length(target_score) != 1L || is.na(target_score)) {
    stop("`target_score` must be a single non-missing numeric value.",
         call. = FALSE)
  }

  ## ---- extract theta values from names like 'theta=0.5' ----
  nm <- names(icc_list)
  if (is.null(nm)) {
    stop("`icc_list` must be a named list with names like 'theta=<value>'.",
         call. = FALSE)
  }
  theta_vals <- suppressWarnings(
    as.numeric(sub("^theta=", "", nm))
  )
  if (anyNA(theta_vals)) {
    stop("All names of `icc_list` must be of the form 'theta=<numeric>'.",
         call. = FALSE)
  }
  exp_scores <- vapply(icc_list, expected_score, numeric(1))
  ## sort
  ord <- order(theta_vals)
  theta_vals <- theta_vals[ord]
  exp_scores <- exp_scores[ord]
  ## TCC bounds
  tcc_min<-min(exp_scores)
  tcc_max<-max(exp_scores)
  ## weak monotonicity check
  if (any(diff(exp_scores) < -1e-6)) {
    warning("Expected score is not monotone in theta; inversion may be unstable.")
  }
  theta_min<-range_tcc[1]
  G<-min(exp_scores)
  X<-ceiling(G)
  fX<-stats::approxfun(theta_vals,exp_scores-X,rule = 1)
  theta_X<-stats::uniroot(fX,interval = range(theta_vals),
                          tol = tol,maxiter = 500)$root

  ## non-invertible scores (3PL)
  if(target_score<tcc_min){
    ## linear interpolation below guessinh
    return(theta_min+(target_score/X)*(theta_X-theta_min))
  }

  if(target_score>=tcc_max){
    return(range_tcc[2])
  }

  f <- stats::approxfun(theta_vals, exp_scores - target_score,
                        rule = 1)
  stats::uniroot(f, interval = range(theta_vals),
                 tol = tol,maxiter = 500)$root
}
