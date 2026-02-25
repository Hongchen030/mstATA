#' Example Mixed-Format Item Pool for MST
#'
#' A simulated item pool containing both standalone items and stimulus-based
#' items. This dataset is designed to demonstrate multistage testing (MST)
#' functionality under a mixed-format item pool, including stimulus-based
#' selection, multiple-objective formulations, and multiple-panel assembly.
#'
#' @format A data frame with 1000 rows and the following variables:
#' \describe{
#'   \item{item_id}{Unique identifier for each item.}
#'   \item{enemyitem}{Identifier(s) of items that cannot be administered together (enemy-item information).}
#'   \item{content}{Content area classification (e.g., \code{content 1}, \code{content 2}, \code{content 3}, \code{content 4}).}
#'   \item{dok}{Depth-of-Knowledge (DOK) level (\code{dok 1}, \code{dok 2}, or \code{dok 3}).}
#'   \item{itemtype}{Item type (e.g., \code{MC} for multiple choice, \code{TEI} for technology-enhanced item).}
#'   \item{time}{Estimated item response time.}
#'   \item{discrimination}{Discrimination parameter (\eqn{a}) for the 2PL model.}
#'   \item{difficulty}{Difficulty parameter (\eqn{b}) for the 2PL model.}
#'   \item{model}{Item response theory (IRT) model for all items (\code{2PL}).}
#'   \item{stim}{Identifier of the associated stimulus (if applicable).}
#'   \item{pivot}{Indicator for pivot items. A pivot item is selected if and only if its associated stimulus is selected; non-pivot items are \code{NA}.}
#'   \item{stimtype}{Stimulus type (e.g., \code{graphic-based}, \code{text-based}).}
#'   \item{stimcomplexity}{complexity score of the associated stimulus.}
#' }
#'
#' @usage data(mixed_format_pool)
#'
#' @examples
#' data(mixed_format_pool)
#'
"mixed_format_pool"
