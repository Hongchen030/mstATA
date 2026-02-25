test_that("compute_joint_score_support returns correct supports for simple routing", {

  prev_scores <- 0:6
  next_mod_max <- 4
  lower <- c(-Inf, 3)
  upper <- c(3, Inf)

  res <- compute_joint_score_support(
    prev_scores,
    next_mod_max,
    lower,
    upper
  )

  # Branch 1: prev 0–3, next 0–4 → 0:7
  expect_equal(res[[1]], 0:7)

  # Branch 2: prev 4–6, next 0–4 → 4:10
  expect_equal(res[[2]], 4:10)
})


test_that("compute_joint_score_support handles empty routing branches", {

  prev_scores <- 0:5
  next_mod_max <- 3
  lower <- c(10)
  upper <- c(20)

  res <- compute_joint_score_support(
    prev_scores,
    next_mod_max,
    lower,
    upper
  )

  expect_length(res, 1)
  expect_equal(res[[1]], integer(0))
})


test_that("compute_joint_score_support works with single previous score", {

  prev_scores <- 2
  next_mod_max <- 3
  lower <- c(-Inf)
  upper <- c(Inf)

  res <- compute_joint_score_support(
    prev_scores,
    next_mod_max,
    lower,
    upper
  )

  expect_equal(res[[1]], 2:5)
})


test_that("compute_joint_score_support validates inputs", {

  expect_error(
    compute_joint_score_support(
      prev_scores = c(-1, 0, 1),
      next_mod_max_score = 2,
      lower = c(-Inf),
      upper = c(Inf)
    ),
    "prev_scores"
  )

  expect_error(
    compute_joint_score_support(
      prev_scores = 0:3,
      next_mod_max_score = -1,
      lower = c(-Inf),
      upper = c(Inf)
    ),
    "next_mod_max_score"
  )

  expect_error(
    compute_joint_score_support(
      prev_scores = 0:3,
      next_mod_max_score = 2,
      lower = c(-Inf, 1),
      upper = c(1)
    ),
    "same length"
  )
})
