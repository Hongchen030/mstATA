#'Example mini item pool for MST
#'
#' A simulated mini item pool for demonstrating MST functionality.
#'
#' @format A data frame with 30 rows and 7 columns:
#' \describe{
#'   \item{item_id}{Unique identifier for each item}
#'   \item{content}{Content area classification (e.g., \code{content1}, \code{content2}, \code{content3}, \code{content4}).}
#'   \item{itemtype}{Item format (e.g., \code{MC}, \code{TEI}).}
#'   \item{time}{Expected response time for the item.}
#'   \item{discrimination}{Discrimination parameter (\eqn{a}) for the 3PL model.}
#'   \item{difficulty}{Difficulty parameter (\eqn{b}) for the 3PL model.}
#'   \item{guessing}{Guessing parameter (\eqn{c}) for the 3PL model.}
#'   \item{model}{Item response theory (IRT) model used for the item (3PL).}
#'   \item{nrCat}{Integer specifying the maximum number of response categories generated for the item.}
#'   \item{enemy_cluing}{enemy information 2}
#'   \item{enemy_similarity}{enemy information 1}
#'   \item{stimulus}{associated stimulus information}
#'   \item{iif(theta=-1)}{item information value at -1}
#'   \item{iif(theta=0)}{item information value at 0}
#'   \item{iif(theta=1)}{item information value at 1}
#' }
#' @usage data(mini_itempool)
#' @examples
#' data(mini_itempool)
#'
"mini_itempool"
