#' @title  Module score distribution at one or more ability values
#'
#' @description
#'
#' Compute the conditional distribution of total module scores given
#' one or more ability values (\eqn{\theta}). by convolving item-level score category
#' probabilities.
#'
#' This function assumes that item category probabilities have already been
#' computed (e.g., via \code{compute_icc()}) and are provided on a common
#' category scale.
#'
#' @param icc A named list of item-by-category probability matrices evaluated
#'   at one or more ability values. Each element corresponds to a single
#'   ability value \eqn{\theta} and must be a numeric matrix with rows
#'   representing items and columns representing score categories
#'   (\code{cat0}, \code{cat1}, \dots).
#'
#'   For mixed dichotomous and polytomous modules, dichotomous items must be
#'   padded so that all matrices share the same set of category columns, with
#'   higher-category probabilities set to zero.
#'
#'
#' @details
#'
#' Consider a module consisting of \eqn{I} items. Let the score for item
#' \eqn{i} be a discrete random variable \eqn{Y_i} taking values
#' \eqn{\{0,1,\dots,m_i\}}. Conditional on ability \eqn{\theta}, assume
#' local independence:
#'
#' \deqn{
#' \Pr(Y_1,\dots,Y_I \mid \theta)
#'   = \prod_{i=1}^{I} \Pr(Y_i \mid \theta).
#' }
#'
#' The total module score is
#'
#' \deqn{
#' S = \sum_{i=1}^{I} Y_i,
#' }
#'
#' with support \eqn{\{0,\dots,\sum_i m_i\}}. For a fixed ability level
#' \eqn{\theta}, the conditional distribution of \eqn{S} is obtained by
#' discrete convolution of the item-level category probability vectors:
#'
#' \deqn{
#' \Pr(S = s \mid \theta)
#'   = (p_{1} * p_{2} * \cdots * p_{I})(s),
#' }
#'
#' where \eqn{p_i = (p_{i0}(\theta),\dots,p_{im_i}(\theta))} and \eqn{*}
#' denotes convolution.
#'
#' This function applies the above operation independently at each
#' ability level.
#'
#'
#' @return
#' A numeric matrix giving conditional module score distributions at target theta values.
#' Rows correspond to total scores
#' \eqn{s = 0,\dots,\sum_i m_i}. Columns correspond to ability values
#' \eqn{\theta}. Each column sums to one.
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
#' module_score_dist(icc_mat[[1]])
#' module_score_dist(icc_list)
#'
#' ## Example 2: Polytomous model (PCM)
#' data("poly_itempool")
#' icc_mat<-compute_icc(poly_itempool,list("PCM"=c("deltaj1","deltaj2","deltaj3")),
#'             theta = 0,model_col = "model")
#' icc_list<-compute_icc(poly_itempool,list("PCM"=c("deltaj1","deltaj2","deltaj3")),
#'             theta = seq(-5,5,0.1),model_col = "model")
#' module_score_dist(icc_mat[[1]])
#' module_score_dist(icc_list)
#'
#' ## Example 3: multiple IRT models (3PL + GPCM + GRM)
#' data("Rmst_pool")
#' icc_mat<-compute_icc(Rmst_pool,item_par_cols = list("3PL"=c("a","b","c"),
#'                                            "GPCM" = c("alpha","delta1","delta2","delta3"),
#'                                             "GRM" = c("alpha","beta1","beta2")),
#'             theta = 0,model_col = "model")
#' icc_list<-compute_icc(Rmst_pool,item_par_cols = list("3PL"=c("a","b","c"),
#'                                            "GPCM" = c("alpha","delta1","delta2","delta3"),
#'                                             "GRM" = c("alpha","beta1","beta2")),
#'             theta = 0,model_col = "model")
#' module_score_dist(icc_mat[[1]])
#' module_score_dist(icc_list)
#' @export

module_score_dist <- function(icc) {
  tol<-1e-10
  ## helper: convolution at one theta
  score_dist_at_theta <- function(icc_mat) {
    if (!is.matrix(icc_mat) || nrow(icc_mat) < 1) {
      stop("Each element of `icc` must be a non-empty matrix.", call. = FALSE)
    }

    score_dist <- 1
    for (i in seq_len(nrow(icc_mat))) {
      score_dist <- convolve_probs(score_dist, icc_mat[i, ])
    }
    score_dist[score_dist<0 & abs(score_dist)<tol] <- 0
    s <- sum(score_dist)
    if (s <= tol) {
      stop("Degenerate module score distribution (sum close to 0). ",
           "Check ICC probabilities.", call. = FALSE)
    }
    score_dist <- score_dist / s

    return(score_dist)
  }

  if (is.matrix(icc)) {
    d <- score_dist_at_theta(icc)
    if (abs(sum(d) - 1) > tol) {
      stop("Module score distribution does not sum to 1.",
           call. = FALSE)
    }
    out <- matrix(d,ncol = 1L,
                  dimnames = list(0:(length(d) - 1L),"theta"))
    return(out)
  }


  if(is.list(icc)){
    if (length(icc) == 0) {
      stop("`icc` must be a non-empty list.", call. = FALSE)
    }
    if (is.null(names(icc))) {
      stop("icc must be a named list with names like 'theta=<value>'.",
           call. = FALSE)
    }
    score_dists <- lapply(icc, score_dist_at_theta)
    bad_cols <- vapply(score_dists,function(d) abs(sum(d) - 1) > tol,logical(1))

    if (any(bad_cols)) {
      stop("Module score distribution does not sum to 1 for: ",
           paste(names(icc)[bad_cols], collapse = ", "),
           call. = FALSE)
    }
    max_score <- max(lengths(score_dists)) - 1L

    out <- matrix(0, nrow = max_score + 1L,
                  ncol = length(score_dists),
                  dimnames = list(0:max_score,names(icc)))

    for (j in seq_along(score_dists)) {
      out[seq_along(score_dists[[j]]), j] <- score_dists[[j]]
    }
    return(out)
  }

  stop("`icc` must be either a matrix or a named list of matrices.",
       call. = FALSE)
}
