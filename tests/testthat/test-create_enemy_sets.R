test_that("create_enemy_sets returns empty when no exclusions", {
  id <- c("A", "B", "C")
  info <- c("", "", "")
  res <- create_enemy_sets(id, info)
  expect_equal(nrow(res$ExclusionPair), 0)
  expect_equal(length(res$EnemySet), 0)
})

test_that("create_enemy_sets builds correct pairs", {
  id <- c("A", "B", "C")
  info <- c("B,C", "", "A")

  res <- create_enemy_sets(id, info)

  expect_equal(
    res$ExclusionPair,
    matrix(c("A","B","A","C"), ncol = 2, byrow = TRUE))

  expect_equal(length(res$EnemySet), 1)
  expect_equal(unlist(res$EnemySet[[1]]),c("A","B","C"))
})

test_that("self pairing is detected", {
  id <- c("A", "B")
  info <- c("A", "")
  expect_error(
    create_enemy_sets(id, info),
    "paired with themselves"
  )
})

test_that("unknown ID is detected", {
  id <- c("A", "B")
  info <- c("C", "")
  expect_error(
    create_enemy_sets(id, info),
    "not in the item pool"
  )
})
