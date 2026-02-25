#' @title Validate objective strategy arguments
#'
#' @description
#'
#' Internal helper that validates and validates user-supplied arguments for
#' multi-objective strategies used in `objective_set()`.
#'
#' Supported strategies:
#' * `"weighted_sum"` – requires positive numeric `weights` (length = #terms).
#' * `"capped_maximin"` – requires:
#'       - all terms must be *relative* (goal = NULL),
#'       - positive numeric `proportions` (length = #terms).
#' * `"maximin"` – requires:
#'       - all terms must be *relative* (goal = NULL),
#'       - positive numeric `proportions` (length = #terms),
#'       - numeric `delta` (length = 1 or = #terms), each > 0 or Inf.
#' * `"goal_programming"` – all terms must be *absolute* (goal not NULL),
#'       requires positive numeric `weights` (length = #terms),
#'       optional mode `"one_dev"` or `"two_dev"`.
#' * `"minimax"` – optional mode `"one_dev"` or `"two_dev"`.
#'
#' @param strategy Character scalar.
#' @param strategy_args User-supplied list of arguments.
#' @param n_terms number of objective terms
#' @param goals Optional numeric vector. If supplied, the objective minimizes the
#' absolute deviation from the goals.Default is NULL.
#' @return A validated list containing only the arguments relevant to the strategy.
#' @keywords internal
#' @noRd

validate_strategy_args <- function(strategy, strategy_args = list(), n_terms,goals = NULL) {
  if (n_terms == 1L) {
    stop("Multiple-term strategies require at least two terms.",call. = FALSE)
  }


  `%||%` <- function(a, b) if (is.null(a)) b else a

  # Helper for validating positive numeric vectors
  validate_positive <- function(input, num_terms, msg) {
    if (!is.numeric(input) || length(input) != num_terms || length(input) ==1 || any(!is.finite(input)) || any(input <= 0)) {
      stop(msg, call. = FALSE)
    }
  }

  # Helper for validating delta
  validate_delta <- function(delta, num_terms) {
    if (length(delta) == 1L) delta <- rep(delta, num_terms)
    if (length(delta) != num_terms) {
      stop("maximin: 'delta' must be length 1 or length = number of terms.", call. = FALSE)
    }
    # Allow Inf but not NA/NaN, must be > 0
    if (!is.numeric(delta) || any(delta <= 0) || any(is.nan(delta))) {
      stop("maximin: 'delta' must be positive numeric and may include Inf.", call. = FALSE)
    }
    delta
  }

  allowed_names <- switch(
    strategy,
    "weighted_sum"    = c("weights","sense"),
    "capped_maximin"  = c("proportions"),
    "maximin"         = c("proportions", "delta"),
    "goal_programming"= c("weights", "mode"),
    "minimax"         = c("mode"),
    stop("Unknown strategy supplied to validate_strategy_args().", call. = FALSE)
  )

  if (!is.list(strategy_args)) {
    stop("strategy_args must be a list.", call. = FALSE)
  }

  if (length(strategy_args) == 0) {
    strategy_args <- as.list(setNames(rep(list(NULL), length(allowed_names)),
                                      allowed_names))
  }

  if (is.null(names(strategy_args))) {
    stop("strategy_args must be a named list.", call. = FALSE)
  }

  if (!all(names(strategy_args) %in% allowed_names)) {
    bad <- names(strategy_args)[!names(strategy_args) %in% allowed_names]
    stop(sprintf("Invalid strategy_args name(s) for strategy '%s': %s.\nAllowed names: %s.",
                 strategy,
                 paste(bad, collapse = ", "),
                 paste(allowed_names, collapse = ", ")),
         call. = FALSE)
  }

  # Switch logic ensures no partial matching
  out <- switch(
    strategy,

    ###########################################################################
    # WEIGHTED SUM
    ###########################################################################
    "weighted_sum" = {
      weights <- strategy_args$weights %||% rep(1, n_terms)
      validate_positive(input = weights,num_terms =  n_terms,
                        msg = "weighted_sum: 'weights' must be positive numeric with length = number of terms.")
      list(weights = weights)
    },

    ###########################################################################
    # CAPPED MAXIMIN
    ###########################################################################
    "capped_maximin" = {
      if (!all(is.na(goals)))   {
        stop("capped_maximin: all terms must be relative objectives (goal = NULL).", call. = FALSE)
      }
      proportions <- strategy_args$proportions %||% rep(1, n_terms)
      if(length(proportions)==1){
        proportions<-rep(proportions,n_terms)
      }
      validate_positive(input = proportions, num_terms = n_terms,
                        msg = "capped_maximin: 'proportions' must be positive numeric with length = number of terms.")
      list(proportions = proportions)
    },

    ###########################################################################
    # MAXIMIN
    ###########################################################################
    "maximin" = {
      # Must be all relative objectives
      if (!all(is.na(goals)))   {
        stop("maximin: all terms must be relative objectives (goal = NULL).", call. = FALSE)
      }
      proportions <- strategy_args$proportions %||% rep(1, n_terms)
      if(length(proportions)==1){
        proportions<-rep(proportions,n_terms)
      }
      validate_positive(input = proportions, num_terms = n_terms,
                        msg = "maximin: 'proportions' must be positive numeric with length = number of terms.")

      delta <- strategy_args$delta %||% Inf
      delta <- validate_delta(delta, n_terms)

      list(proportions = proportions, delta = delta)
    },

    ###########################################################################
    # GOAL PROGRAMMING
    ###########################################################################
    "goal_programming" = {
      # Must be all absolute objectives
      if (any(is.na(goals))|| length(goals) != n_terms) {
        stop("goal_programming: all terms must be absolute objectives (goal must be specified).",
             call. = FALSE)
      }
      weights <- strategy_args$weights %||% rep(1, n_terms)
      if(length(weights) == 1){
        weights<-rep(weights,n_terms)
      }
      validate_positive(weights, n_terms,
                        "goal_programming: 'weights' must be positive numeric with length = number of terms.")
      mode <- match.arg(strategy_args$mode %||% "one_dev", c("one_dev","two_dev"))
      list(mode = mode, weights = weights)
    },

    ###########################################################################
    # MINIMAX
    ###########################################################################
    "minimax" = {
      if (any(is.na(goals))|| length(goals) != n_terms) {
        stop("minimax: all terms must be absolute objectives (goal must be specified).",
             call. = FALSE)
      }
      mode <- match.arg(strategy_args$mode %||% "one_dev", c("one_dev","two_dev"))
      list(mode = mode)
    },

    ###########################################################################
    # UNKNOWN STRATEGY
    ###########################################################################
    stop("Unknown strategy supplied to validate_strategy_args().", call. = FALSE)
  )

  return(out)
}
