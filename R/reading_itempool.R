#' @title Reading Item Pool
#' @description
#'
#' A reading item pool for demonstrating MST functionality.
#'
#' @format A data frame with 500 rows and 15 columns:
#' \describe{
#'   \item{item_id}{Unique identifier for each item.}
#'   \item{content}{Content area classification (e.g., \code{content1}, \code{content2}, \code{content3}, \code{content4}).}
#'   \item{itemtype}{Item format (e.g., \code{MC}, \code{TEI}).}
#'   \item{time}{Expected response time for the item.}
#'   \item{discrimination}{Discrimination parameter (\eqn{a}) for the 3PL model.}
#'   \item{difficulty}{Difficulty parameter (\eqn{b}) for the 3PL model.}
#'   \item{guessing}{Guessing parameter (\eqn{c}) for the 3PL model.}
#'   \item{model}{Item response theory (IRT) model used for the item (3PL).}
#'   \item{nrCat}{Integer specifying the maximum number of response categories generated for the item.}
#'   \item{enemy_item}{enemy item pairs/sets information}
#'   \item{stimulus}{associated stimulus information}
#'   \item{pivot_item}{the indicator of pivot items. A pivot item is defined as an item that is always selected for the test if and only if its stimulus is selected}
#'   \item{enemy_stimulus}{enemy stimulus pairs/sets information}
#'   \item{stimulus_type}{Stimulus types: history, social studies}
#'   \item{stimulus_words}{Stimulus words}
#' }
#' @usage data(reading_itempool)
#' #' @examples
#' data(reading_itempool)
"reading_itempool"



