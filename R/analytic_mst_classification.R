#' @title Analytic classification accuracy and consistency for MST panels
#'
#' @description
#'
#' Once the analytical CSEMs are derived based on recursion-based approach (Lim, 2000),
#' the classification accuracy of MST can be estimated using Rudner's method (2000,2005).
#' The implementation follows the same logic as \code{irtQ::cac_rud()}.
#'
#' @param decision_theta_cuts Numeric vector of classification cut theta values.
#'   These define \eqn{K+1} ordered classification levels.
#'
#' @param eval_tb A data frame containing at least the columns \code{theta} and \code{csem}.
#' It can be created by \code{analytic_mst_precision()} or \code{analytic_mst_precision_items()}
#'
#' @param theta_weight A data frame containing the population distribution
#'   over the ability grid. It must have the following columns:
#'   \describe{
#'     \item{theta}{Numeric ability grid. This must match
#'       \code{eval_tb$theta} exactly.}
#'     \item{w}{Non-negative population weights corresponding to each
#'       ability value.}
#'   }
#'   The \code{theta_weight} object can be generated using
#'   \code{\link{gen_weight}}.
#'
#' @details
#' This function evaluates classification performance without simulation.
#' Given a set of decision cut scores and conditional standard errors across
#' a fixed ability grid, classification accuracy and consistency are computed
#' analytically by integrating conditional classification probabilities over
#' a population ability distribution.
#'
#' The procedure consists of the following steps:
#'
#' \enumerate{
#'   \item \strong{Define true classification levels:}
#'   Each ability value \eqn{\theta} on the evaluation grid is assigned to a
#'   true classification level based on the specified cut scores.
#'
#'   \item \strong{Compute conditional classification probabilities:}
#'   For each \eqn{\theta}, the probability of being classified into each level
#'   is computed assuming a normal distribution for the reported ability
#'   estimate:
#'   \deqn{
#'     \hat{\theta} \mid \theta \sim \mathcal{N}(\theta,\ \mathrm{CSEM}(\theta)^2).
#'   }
#'
#'   \item \strong{Conditional accuracy and consistency:}
#'
#'   Conditional classification accuracy is defined as
#'   \deqn{
#'     P(\hat{C} = C \mid \theta),
#'   }
#'   where \eqn{C} is the true classification level.
#'
#'   Conditional consistency is defined as
#'   \deqn{
#'     \sum_k P(\hat{C} = k \mid \theta)^2,
#'   }
#'   representing the probability that two independent parallel administrations
#'   yield the same classification.
#'
#'   \item \strong{Marginalization over the population:}
#'
#'   Conditional indices are integrated over a population ability distribution
#'   using externally supplied weights, yielding marginal classification
#'   accuracy, marginal classification consistency, and a confusion matrix.
#' }
#'
#'
#' @return
#' A list with the following components:
#' \describe{
#'   \item{confusion}{A matrix giving the joint probability of true and expected
#'     classification levels. Rows correspond to true levels; columns correspond
#'     to expected levels.}
#'   \item{marginal}{A data frame of marginal classification accuracy and
#'     consistency by true level, including an overall (marginal) row.}
#'   \item{conditional}{A data frame of conditional accuracy and consistency
#'     at each ability value \eqn{\theta}.}
#'   \item{prob.level}{A data frame of conditional classification probabilities
#'     for each level at each \eqn{\theta}.}
#'   \item{cutscore}{The classification cut scores used in the analysis.}
#' }
#'
#' @section Mathematical Formulation:
#'
#' Let \eqn{\theta_i} denote an ability grid point and
#' \eqn{\mathrm{CSEM}(\theta_i)} the corresponding conditional standard error.
#' For classification cut scores \eqn{c_1, \dots, c_K}, define level boundaries
#' \eqn{(-\infty, c_1, \dots, c_K, \infty)}.
#'
#' The probability of classifying an examinee with true ability \eqn{\theta_i}
#' into level \eqn{k} is
#' \deqn{
#'   P(\hat{C}=k \mid \theta_i) =
#'   \Phi\!\left(\frac{u_k - \theta_i}{\mathrm{CSEM}(\theta_i)}\right)
#'   -
#'   \Phi\!\left(\frac{\ell_k - \theta_i}{\mathrm{CSEM}(\theta_i)}\right),
#' }
#' where \eqn{(\ell_k, u_k)} is the interval for level \eqn{k}.
#'
#' Conditional accuracy at \eqn{\theta_i} is
#' \deqn{
#'   P(\hat{C}=C \mid \theta_i),
#' }
#' and conditional consistency is
#' \deqn{
#'   \sum_{k=1}^{K+1} P(\hat{C}=k \mid \theta_i)^2.
#' }
#'
#' Let \eqn{w(\theta_i) \ge 0} be population weights on the grid, with
#' \eqn{\sum_{i=1}^{I} w(\theta_i)=1}. The marginal (population) indices are the
#' discrete weighted averages of the conditional quantities:
#'
#' \deqn{
#' \mathrm{MarginalAccuracy}
#' \;=\;
#' \sum_{i=1}^{I} w(\theta_i)\;
#' P\!\left(\hat{C}=C(\theta_i)\mid \theta_i\right),
#' }
#' where \eqn{C(\theta_i)} is the true level implied by \eqn{\theta_i} and the cut
#' scores.
#'
#' \deqn{
#' \mathrm{MarginalConsistency}
#' \;=\;
#' \sum_{i=1}^{I} w(\theta_i)\;
#' \sum_{k=1}^{K+1} P\!\left(\hat{C}=k \mid \theta_i\right)^2.
#' }
#'
#' If \eqn{w(\theta_i)} approximates a density \eqn{g(\theta)}, these correspond to
#' the integrals \eqn{\int g(\theta)\,\cdot\, d\theta}.
#'
#'
#' @references
#' Rudner, L. M. (2000). Computing the expected proportions of misclassified examinees.
#' \emph{Practical Assessment, Research, and Evaluation}, 7(1).
#' {doi:10.7275/an9m-2035}
#'
#' Rudner, L. M. (2005). Expected classification accuracy.
#' \emph{Practical Assessment, Research, and Evaluation}, 10(1).
#' {doi:10.7275/56a5-6b14}
#'
#' @seealso
#' [gen_weight()],
#' [analytic_mst_precision()],
#' [analytic_mst_precision_items()]
#'
#' @export

analytic_mst_classification<-function(decision_theta_cuts,eval_tb,
                                      theta_weight){

  if (!is.numeric(decision_theta_cuts)) {
    stop("`decision_theta_cuts` must be a numeric vector of classification cut scores.",
         call. = FALSE)
  }
  if (any(diff(decision_theta_cuts) <= 0)) {
    stop("`decision_theta_cuts` must be strictly increasing.", call. = FALSE)
  }
  if (any(!is.finite(decision_theta_cuts))) {
    stop("`decision_theta_cuts` must be finite.", call. = FALSE)
  }

  if (is.null(eval_tb)) {
    stop(
      "`eval_tb` (a data frame with columns theta and csem) must be provided ",
      "and can be obtained from analytic_mst_precision().",
      call. = FALSE
    )
  }

  if(is.null(theta_weight)){
    stop(
      "`theta_weight` (a data frame with columns theta and w) must be provided ",
      "and can be obtained from gen_weight().",
      call. = FALSE
    )
  }
  eval_tb<-as.data.frame(eval_tb)
  theta_weight<-as.data.frame(theta_weight)

  if (!all(c("theta", "csem") %in% colnames(eval_tb))) {
    stop("`eval_tb` must contain columns named 'theta' and 'csem'.",
         call. = FALSE)
  }
  theta<-eval_tb$theta
  csem<-eval_tb$csem
  if (any(csem <= 0 | !is.finite(csem))) {
    stop("`csem` must be finite and strictly positive at all theta values.",
         call. = FALSE)
  }
  if (!all(c("theta", "w") %in% colnames(theta_weight))) {
    stop("`theta_weight` must be a dataframe with columns 'theta' and 'w'.",
         call. = FALSE)
  }
  nodes <- theta_weight$theta
  wts <- theta_weight$w
  if (!isTRUE(all.equal(nodes, theta))) {
    stop("The theta grid in `theta_weight` must match eval_tb$theta.",
         call. = FALSE)
  }
  if (any(wts < 0)) {
    stop("Population weights must be non-negative.", call. = FALSE)
  }
  sw <- sum(wts)
  if (sw <= 0) {
    stop("Population weights must sum to a positive value.", call. = FALSE)
  }
  wts <- wts / sw

  num_theta_points <- length(wts)
  num_classes <- length(decision_theta_cuts) + 1
  levels_all<-seq_len(num_classes)
  breaks <- c(-Inf, decision_theta_cuts, Inf)
  true_class <- cut(x = nodes, breaks = breaks, labels = FALSE,
                    include.lowest = TRUE, right = FALSE, dig.lab = 7)
  cond_tb <- data.frame(theta = nodes, weights = wts, level = true_class,
                        accuracy = NA_real_, consistency = NA_real_)
  ps_tb <- matrix(NA, nrow = num_theta_points, ncol = num_classes)
  colnames(ps_tb) <- paste0("p.level.", levels_all)
  # Matrix: rows = theta points, cols = class boundaries
  cum_ps <- sapply(breaks,function(b) stats::pnorm(q = b, mean = nodes, sd = csem))
  ps_tb <- t(apply(cum_ps, 1, diff))
  # numerical stabilization
  ps_tb <- pmax(ps_tb, 0)
  ps_tb <- ps_tb / rowSums(ps_tb)
  cond_tb$accuracy    <- ps_tb[cbind(seq_len(num_theta_points), true_class)]
  cond_tb$consistency <- rowSums(ps_tb^2)


  acc_by_level <- numeric(num_classes)
  con_by_level <- numeric(num_classes)
  acc_by_level <- tapply(cond_tb$accuracy * cond_tb$weights,
                         true_class,sum,default = 0)
  con_by_level <- tapply(cond_tb$consistency * cond_tb$weights,
                         true_class,sum,default = 0)

  margin_tb <- data.frame(level = levels_all,
                          accuracy = acc_by_level,
                          consistency = con_by_level)

  ## add marginal (overall) row
  margin_tb <- rbind(margin_tb,
                     data.frame(level = "marginal",
                                accuracy = sum(cond_tb$accuracy * cond_tb$weights),
                                consistency = sum(cond_tb$consistency * cond_tb$weights)))
  row.names(margin_tb) <- NULL

  ## ---- confusion matrix (base R) ----
  cross_tb <- matrix(0, nrow = num_classes, ncol = num_classes)
  for (k in levels_all) {
    idx <- true_class==k
    if (any(idx)) {
      cross_tb[k, ] <- colSums(ps_tb[idx, , drop = FALSE] * wts[idx])
    }
  }

  dimnames(cross_tb) <- list(True = levels_all,
                             Expected = levels_all)
  rst <- list(cutscore = decision_theta_cuts,
              confusion = cross_tb,
              marginal = margin_tb,
              conditional = cond_tb,
              prob.level = ps_tb)
  return(rst)
}
