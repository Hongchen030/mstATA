#' @title Normalize routing cuts to the score scale
#'
#' @description
#'
#' Converts routing cuts to the number-correct (score) scale so that
#' they can be used consistently in score-based routing logic for
#' multistage testing (MST).
#'
#' @details
#'
#' Routing decisions in recursion-based evaluation method are based on observed scores (number correct).
#' When cut scores are specified on the ability (\eqn{\theta}) scale, they
#' must be converted to equivalent score-scale cut points using the test
#' characteristic curve (TCC) implied by the items already administered.
#'
#' If \code{cut_scale = "score"}, the input cut scores are assumed to already
#' be on the score scale and are returned unchanged after validation.
#'
#' If \code{cut_scale = "theta"}, each cut value \eqn{\theta_c} is converted
#' to a score-scale cut point via
#'
#' \deqn{
#' c = E(S \mid \theta = \theta_c),
#' }
#'
#' where \eqn{S} denotes the cumulative score and the expectation is computed
#' by interpolating the test characteristic curve derived from
#' \code{icc_list}. The accuracy of this conversion depends on the density
#' and range of the ability grid used to construct \code{icc_list}.
#'
#' The returned cut scores are on the score scale and may be non-integer.
#' Observed scores are integers and are compared against these (possibly
#' fractional) cut points during routing.
#'
#' @param cut_scale Character string indicating the scale of \code{cuts}.
#'   Must be one of \code{"score"} or \code{"theta"}. Default is \code{"score"}.
#'
#' @param cuts A numeric vector of internal cut scores. For
#'   \code{"score"}, these are number-correct cut points; for
#'   \code{"theta"}, these are ability cut points. Values must be strictly
#'   increasing.
#'
#' @param icc_list A named list of item category probability matrices
#'   evaluated on an ability grid. Required when
#'   \code{cut_scale = "theta"}.
#'
#' @return
#' A numeric vector of cut scores on the score scale.
#'
#' @examples
#' ## score-scale cuts (returned unchanged)
#' normalize_cut_scores("score", cuts = c(5, 10))
#'
#' ## theta-scale cuts converted to score scale
#' icc_list<-compute_icc(mini_itempool,list("3PL"=c("discrimination","difficulty","guessing")),
#'             theta = seq(-5,5,0.1),model_col = "model",D = 1)
#' normalize_cut_scores(
#'   "theta",
#'   cuts = c(-0.5, 0.8),
#'   icc_list = icc_list
#' )
#'
#' @seealso [expected_score()]
#'
#' @export

normalize_cut_scores <- function(cut_scale = "score",
                                 cuts,
                                 icc_list = NULL) {
  cut_scale <- match.arg(cut_scale, c("score", "theta"))

  if (!is.numeric(cuts) || length(cuts) == 0L || anyNA(cuts)) {
    stop("`cuts` must be a non-empty numeric vector with no NA values.",
         call. = FALSE)
  }

  if (any(diff(cuts) <= 0)) {
    stop("Cut scores must be strictly increasing.", call. = FALSE)
  }

  if (cut_scale == "score") {
    return(as.numeric(cuts))
  }

  if (cut_scale == "theta") {
    if (is.null(icc_list)) {
      stop("`icc_list` must be provided when cut_scale = 'theta'.",
           call. = FALSE)
    }

    if (!is.list(icc_list) || is.null(names(icc_list))) {
      stop("`icc_list` must be a named list when cut_scale = 'theta'.",
           call. = FALSE)
    }

    score_cuts <- vapply(
      cuts,
      function(theta_cut) expected_score(icc_list, theta_cut),
      numeric(1)
    )
    return(as.numeric(score_cuts))
  }
}
