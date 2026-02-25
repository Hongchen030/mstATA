test_that("validate_enemy_set_index succeeds for valid enemy indices", {

  itempool <- data.frame(item_id = letters[1:5])

  enemy_set <- list(
    ItemIndex = list(
      c(1L, 2L),
      c(3L, 4L, 5L)
    )
  )

  expect_true(
    validate_enemy_set_index(enemy_set, itempool)
  )
})

test_that("validate_enemy_set_index fails if ItemIndex is missing", {

  itempool <- data.frame(item_id = letters[1:3])

  enemy_set <- list()

  expect_error(
    validate_enemy_set_index(enemy_set, itempool),
    "ItemIndex"
  )
})

test_that("validate_enemy_set_index fails if indices are not integer", {

  itempool <- data.frame(item_id = letters[1:4])

  enemy_set <- list(
    ItemIndex = list(c(1, 2))  # numeric, not integer
  )

  expect_error(
    validate_enemy_set_index(enemy_set, itempool),
    "integer"
  )
})

test_that("validate_enemy_set_index fails if indices contain NA", {

  itempool <- data.frame(item_id = letters[1:4])

  enemy_set <- list(
    ItemIndex = list(c(1L, NA_integer_))
  )

  expect_error(
    validate_enemy_set_index(enemy_set, itempool),
    "NA"
  )
})

test_that("validate_enemy_set_index fails if indices are outside itempool", {

  itempool <- data.frame(item_id = letters[1:3])

  enemy_set <- list(
    ItemIndex = list(c(1L, 4L))
  )

  expect_error(
    validate_enemy_set_index(enemy_set, itempool),
    "outside"
  )
})



