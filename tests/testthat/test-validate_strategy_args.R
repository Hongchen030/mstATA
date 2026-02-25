# -------------------------------------------------------------------------
# weighted_sum
# -------------------------------------------------------------------------

test_that("weighted_sum validates weights and sense", {
  out <- validate_strategy_args(
    strategy = "weighted_sum",
    strategy_args = list(weights = c(1, 2)),
    n_terms = 2
  )
  expect_equal(out$weights, c(1, 2))

  out <- validate_strategy_args(
    strategy = "weighted_sum",
    n_terms = 3
  )
  expect_equal(out$weights, c(1, 1, 1))
})

test_that("weighted_sum errors on invalid weights", {
  expect_error(
    validate_strategy_args(
      strategy = "weighted_sum",
      strategy_args = list(weights = c(1, -1)),
      n_terms = 2
    ),
    "weights"
  )
})

# -------------------------------------------------------------------------
# capped_maximin
# -------------------------------------------------------------------------

test_that("capped_maximin only allows relative objectives", {
  out <- validate_strategy_args(
    strategy = "capped_maximin",
    n_terms = 2
  )
  expect_equal(out$proportions, c(1, 1))

  expect_error(
    validate_strategy_args(
      strategy = "capped_maximin",
      goals = c(0, 0),
      n_terms = 2
    ),
    "relative objectives"
  )
})

# -------------------------------------------------------------------------
# maximin
# -------------------------------------------------------------------------

test_that("maximin validates proportions and delta", {
  out <- validate_strategy_args(
    strategy = "maximin",
    strategy_args = list(
      proportions = c(1, 2),
      delta = 5
    ),
    n_terms = 2
  )
  expect_equal(out$proportions, c(1, 2))
  expect_equal(out$delta, c(5, 5))
})

test_that("maximin rejects absolute objectives", {
  expect_error(
    validate_strategy_args(
      strategy = "maximin",
      goals = c(1, 2),
      n_terms = 2
    ),
    "relative objectives"
  )
})

test_that("maximin errors on invalid delta", {
  expect_error(
    validate_strategy_args(
      strategy = "maximin",
      strategy_args = list(delta = c(1, -1)),
      n_terms = 2
    ),
    "delta"
  )
})

# -------------------------------------------------------------------------
# goal_programming
# -------------------------------------------------------------------------

test_that("goal_programming requires absolute objectives", {
  out <- validate_strategy_args(
    strategy = "goal_programming",
    strategy_args = list(weights = c(1, 2), mode = "two_dev"),
    n_terms = 2,
    goals = c(10, 20)
  )
  expect_equal(out$weights, c(1, 2))
  expect_equal(out$mode, "two_dev")

  expect_error(
    validate_strategy_args(
      strategy = "goal_programming",
      n_terms = 2,
      goals = NA_real_
    ),
    "goal must be specified"
  )

})

# -------------------------------------------------------------------------
# minimax
# -------------------------------------------------------------------------

test_that("minimax requires absolute objectives", {
  out <- validate_strategy_args(
    strategy = "minimax",
    strategy_args = list(mode = "one_dev"),
    n_terms = 2,
    goals = c(5, 10)
  )
  expect_equal(out$mode, "one_dev")

  expect_error(
    validate_strategy_args(
      strategy = "minimax",
      n_terms = 2
    ),
    "absolute objectives"
  )
})

# -------------------------------------------------------------------------
# argument name validation
# -------------------------------------------------------------------------

test_that("invalid strategy_args names are rejected", {
  expect_error(
    validate_strategy_args(
      strategy = "single",
      strategy_args = list(foo = 1),
      n_terms = 1
    ),
    "Multiple-term strategies require at least two terms."
  )
})

test_that("strategy_args must be a named list", {
  expect_error(
    validate_strategy_args(
      strategy = "maximin",
      strategy_args = list(1, 2),
      n_terms = 2
    ),
    "named list"
  )
})

# -------------------------------------------------------------------------
# unknown strategy
# -------------------------------------------------------------------------

test_that("unknown strategy errors cleanly", {
  expect_error(
    validate_strategy_args(
      strategy = "unknown",
      n_terms = 2
    ),
    "Unknown strategy"
  )
})
