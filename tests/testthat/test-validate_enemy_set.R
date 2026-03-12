test_that("validate_enemy_set succeeds for a valid enemy_set", {

  enemy_set <- list(
    ExclusionPair = matrix(
      c("A", "B",
        "C", "D"),
      ncol = 2,
      byrow = TRUE
    ),
    EnemySet = list(
      c("A", "B"),
      c("C", "D")
    )
  )

  expect_true(validate_enemy_set(enemy_set))
})

test_that("validate_enemy_set fails when EnemySet introduces undeclared pairs", {

  enemy_set <- list(
    ExclusionPair = matrix(
      c("A", "B",
        "C", "D"),
      ncol = 2,
      byrow = TRUE
    ),
    EnemySet = list(
      c("A", "B"),
      c("C", "D", "E")  # introduces C–E, D–E
    )
  )

  expect_error(
    validate_enemy_set(enemy_set),
    "EnemySet contains items not appearing in ExclusionPair."
  )

  enemy_set <- list(
    ExclusionPair = matrix(
      c("A", "B",
        "C", "D",
        "C","E"),
      ncol = 2,
      byrow = TRUE
    ),
    EnemySet = list(
      c("A", "B"),
      c("C", "D", "E")
    )
  )

  expect_true(validate_enemy_set(enemy_set))
})

test_that("validate_enemy_set fails when ExclusionPair is not represented in EnemySet", {

  enemy_set <- list(
    ExclusionPair = matrix(c("A", "B"), ncol = 2),
    EnemySet = list(c("A", "C"))
  )

  expect_error(
    validate_enemy_set(enemy_set),
    "not represented"
  )
})


test_that("validate_enemy_set fails for duplicated enemy pairs", {

  enemy_set <- list(
    ExclusionPair = matrix(
      c("A", "B",
        "B", "A"),
      ncol = 2,
      byrow = TRUE
    ),
    EnemySet = list(c("A", "B"))
  )

  expect_error(
    validate_enemy_set(enemy_set),
    "duplicated"
  )
})




