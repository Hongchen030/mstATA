test_that("concat_enemy_sets unions pairwise exclusions correctly", {
  enemy_set_1 <- list(
    ExclusionPair = matrix(
      c("A", "B"),
      ncol = 2,
      byrow = TRUE
    ),
    EnemySet = list(c("A", "B"))
  )

  enemy_set_2 <- list(
    ExclusionPair = matrix(
      c("B", "C"),
      ncol = 2,
      byrow = TRUE
    ),
    EnemySet = list(c("B", "C"))
  )

  combined <- concat_enemy_sets(enemy_set_1, enemy_set_2)

  expect_true(is.list(combined))
  expect_true(all(c("ExclusionPair", "EnemySet") %in% names(combined)))

  # "concat_enemy_sets does not infer transitive enemy pairs"
  combined <- concat_enemy_sets(enemy_set_1, enemy_set_2)

  pairs <- apply(combined$ExclusionPair, 1, paste, collapse = "-")

  expect_true("A-B" %in% pairs)
  expect_true("B-C" %in% pairs)
  expect_false("A-C" %in% pairs)

  # "concat_enemy_sets preserves source-level EnemySet structure"
  combined <- concat_enemy_sets(enemy_set_1, enemy_set_2)

  expect_length(combined$EnemySet, 2)
  expect_true(any(vapply(combined$EnemySet,
                         function(s) identical(sort(s), c("A","B")),
                         logical(1))))
  expect_true(any(vapply(combined$EnemySet,
                         function(s) identical(sort(s), c("B","C")),
                         logical(1))))

  # "concat_enemy_sets validates all inputs"
  bad_set <- list(ExclusionPair = matrix("A", ncol = 2))

  expect_error(
    concat_enemy_sets(enemy_set_1, bad_set),
    "missing required components"
  )

  # "concat_enemy_sets requires at least one input"
  expect_error(
    concat_enemy_sets(),
    "At least one enemy_set must be provided"
  )

})
