#' Example Polytomous Item Pool for MST
#'
#' A simulated item pool consisting exclusively of polytomously scored items,
#' designed to demonstrate multistage testing (MST) functionality under a
#' partial credit modeling framework.
#'
#' @format A data frame with 50 rows and the following columns:
#' \describe{
#'   \item{item_id}{Unique identifier for each item.}
#'   \item{content}{Content area classification (e.g., \code{content1}, \code{content2}, \code{content3}, \code{content4}).}
#'   \item{deltaj1}{First step (threshold) parameter in the Partial Credit Model (PCM).}
#'   \item{deltaj2}{Second step (threshold) parameter in the Partial Credit Model (PCM).}
#'   \item{deltaj3}{Third step (threshold) parameter in the Partial Credit Model (PCM).}
#'   \item{model}{Item response theory (IRT) model used for all items (\code{PCM}).}
#'   \item{nrCat}{Integer specifying the maximum number of response categories generated for the item.}
#' }
#'
#' @usage data(poly_itempool)
#'
#' @examples
#' data(poly_itempool)
#'
"poly_itempool"

