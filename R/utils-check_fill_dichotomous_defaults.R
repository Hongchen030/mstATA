#' @keywords internal
#' @noRd

fill_dichotomous_defaults <- function(item_params) {
  item_params <- as.matrix(item_params)

  if (!is.numeric(item_params)) {
    stop("Item parameters must be numeric.", call. = FALSE)
  }

  n <- ncol(item_params)
  n_items<-nrow(item_params)

  if (n == 1L) {
    a <- rep(1,n_items)
    b <- item_params[, 1]
    g <- rep(0,n_items)
    d <- rep(1,n_items)
  } else if (n == 2L) {
    a <- item_params[, 1]
    b <- item_params[, 2]
    g <- rep(0,n_items)
    d <- rep(1,n_items)
  } else if (n == 3L) {
    a <- item_params[, 1]
    b <- item_params[, 2]
    g <- item_params[, 3]
    d <- rep(1,n_items)
  }else if (n == 4L){
    a <- item_params[, 1]
    b <- item_params[, 2]
    g <- item_params[, 3]
    d <- item_params[, 4]
  }else{
    stop("Dichotomous IRT models require 1-4 parameters (b; a,b; a,b,g; a,b,g,d).",
         call. = FALSE)
  }

  if (any(a <= 0, na.rm = TRUE)) {
    stop("Discrimination parameter 'a' should be positive.", call. = FALSE)
  }
  if (anyNA(b)) {
    stop("Difficulty parameter 'b' contains NA.", call. = FALSE)
  }
  if (any(g < 0 | g > 1, na.rm = TRUE)) {
    stop("Guessing parameter 'g' should be between 0 and 1.", call. = FALSE)
  }

  if (any(d < 0 | d > 1, na.rm = TRUE)) {
    stop("Upper asymptote parameter 'd' should be between 0 and 1.", call. = FALSE)
  }

  if (any(d <= g, na.rm = TRUE)) {
    stop("Upper asymptote 'd' must be strictly greater than guessing 'g'.",
         call. = FALSE)
  }

  return(cbind(a = a, b = b, g = g, d = d))
}
