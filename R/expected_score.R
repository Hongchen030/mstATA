#' @title Expected score
#'
#' @description
#' Computes the expected total score implied by item category probability
#' curves. The expectation can be evaluated either at a grid point
#' (single ICC matrix) or at an arbitrary ability value.
#'
#' @param icc Either a numeric matrix of item category
#'   probabilities at a fixed ability level \eqn{\theta} (Rows correspond
#'   to items and columns correspond to response categories
#'   (\code{cat0}, \code{cat1}, \dots).), or a named list of such matrices
#'   evaluated on the ability grid.
#' @param target_theta Optional numeric value giving the ability level at
#'   which the expected score is to be evaluated. Required when \code{icc}
#'   is a list.
#'
#' @details
#'
#' **1. How expected score is computed**
#'
#' Consider there are \eqn{I} items. Let the score for item
#' \eqn{i} be a discrete random variable \eqn{Y_i} taking values
#' \eqn{\{0, 1, \dots, m\}}. Conditional on ability \eqn{\theta},
#' item responses are assumed to be locally independent, with category
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
#' E(Y_i \mid \theta) = \sum_{k=0}^{m} k \, p_{ik}(\theta).
#' }
#'
#' The expected total score is the sum of item-level expectations:
#'
#' \deqn{
#' E(S \mid \theta)
#'   = E\!\left(\sum_{i=1}^{I} Y_i \,\middle|\, \theta\right)
#'   = E\!\left(S \,\middle|\, \theta\right)
#'   = \sum_{i=1}^{I} E(Y_i \mid \theta),
#' }
#'
#' where \eqn{S = \sum_{i=1}^{I} Y_i} denotes the total score.
#'
#' This function evaluates the above expression numerically using the
#' item category probability matrix produced by
#' \code{\link{compute_icc}}. Missing categories (if any) are assumed
#' to have zero probability.
#'
#' **2. ICC input**
#'
#' (1) If \code{icc} is a matrix, the expected score is computed directly as
#'
#' \deqn{
#' E(S \mid \theta) = \sum_{i}\sum_{k} k\,p_{ik}(\theta),
#' }
#'
#' where \eqn{p_{ik}(\theta)} denotes the probability that item \eqn{i}
#' produces score \eqn{k} at ability level \eqn{\theta}.
#'
#' (2) If \code{icc} is a list, the expected score function
#' \eqn{E(S \mid \theta)} is first evaluated on the grid encoded in
#' \code{names(icc)}, and a continuous approximation is constructed via
#' linear interpolation. The expected score at \code{target_theta} is then
#' obtained from this interpolated test characteristic curve using \code{stats::approxfun()}.
#' \strong{The numerical accuracy of the interpolated value depends on the density
#' and range of the ability grid used to construct the ICC list.}
#'
#'
#' @return
#' A single numeric value giving the expected total score
#' \eqn{E(S \mid \theta)} at the specified ability level.
#'
#' @examples
#' ## Example 1: Dichotomous model
#' data("mini_itempool")
#' icc_mat<-compute_icc(mini_itempool,list("3PL"=c("discrimination","difficulty","guessing")),
#'             theta = 0,model_col = "model",D = 1.7)
#'
#' icc_list<-compute_icc(mini_itempool,list("3PL"=c("discrimination","difficulty","guessing")),
#'             theta = seq(-5,5,0.1),model_col = "model",D = 1.7)
#'
#' expected_score(icc_mat[[1]])
#' expected_score(icc_list,target_theta=seq(-5,5,0.1))
#'
#' ## Example 2: Polytomous model (PCM)
#' data("poly_itempool")
#' icc_mat<-compute_icc(poly_itempool,list("PCM"=c("deltaj1","deltaj2","deltaj3")),
#'             theta = 0,model_col = "model")
#' icc_list<-compute_icc(poly_itempool,list("PCM"=c("deltaj1","deltaj2","deltaj3")),
#'             theta = seq(-5,5,0.1),model_col = "model")
#' expected_score(icc_mat[[1]])
#' expected_score(icc_list,target_theta=0)
#'
#' ## Example 3: Mixed item format (3PL + GPCM + GRM)
#' data("Rmst_pool")
#' icc_mat<-compute_icc(Rmst_pool,item_par_cols = list("3PL"=c("a","b","c"),
#'                                            "GPCM" = c("alpha","delta1","delta2","delta3"),
#'                                             "GRM" = c("alpha","beta1","beta2")),
#'             theta = 0,model_col = "model")
#' icc_list<-compute_icc(Rmst_pool,item_par_cols = list("3PL"=c("a","b","c"),
#'                                            "GPCM" = c("alpha","delta1","delta2","delta3"),
#'                                             "GRM" = c("alpha","beta1","beta2")),
#'             theta = seq(-5,5,0.1),model_col = "model")
#' expected_score(icc_mat[[1]])
#' expected_score(icc_list,target_theta=0)
#' @seealso [compute_icc()],[inv_tcc_from_icc()]
#'
#' @export

expected_score <- function(icc,target_theta = NULL) {
  # case 1: single ICC matrix
  if (is.matrix(icc)) {
    probs <- icc
    scores <- seq_len(ncol(probs)) - 1L
    return(sum(rowSums(probs * rep(scores, each = nrow(probs)))))
  }

  if(is.list(icc)){
    if (length(icc) == 0) {
      stop("`icc` must be a non-empty list.", call. = FALSE)
    }
    if (is.null(names(icc))) {
      stop("icc must be a named list with names like 'theta=<value>'.",
           call. = FALSE)
    }

    if(is.null(target_theta)){
      stop("target_theta must be provided when icc is a list.",
           call. = FALSE)
    }

    ## extract theta grid from names
    theta_grid <- suppressWarnings(
      as.numeric(sub("^theta=", "", names(icc)))
    )

    if (anyNA(theta_grid)) {
      stop("Failed to parse theta values from names(icc). ",
           "Expected names like 'theta=<numeric>'.",
           call. = FALSE)
    }

    ## expected score at each theta
    exp_scores <- vapply(
      icc,
      FUN = function(icc_mat) {
        probs <- icc_mat
        scores <- seq_len(ncol(probs)) - 1L
        sum(rowSums(probs * rep(scores, each = nrow(probs))))
      },
      numeric(1)
      )

    ord <- order(theta_grid)
    theta_grid <- theta_grid[ord]
    exp_scores <- exp_scores[ord]

    if (any(diff(theta_grid) <= 0)) {
      stop("Theta values extracted from names(icc) must be strictly increasing.",
           call. = FALSE)
    }


    ## continuous TCC via interpolation
    T_prefix<-stats::approxfun(x = theta_grid,y = exp_scores,rule = 2)
    return(T_prefix(target_theta))
  }

  stop("icc must be either a numeric matrix or a list of ICC matrices.",
         call. = FALSE)
}
