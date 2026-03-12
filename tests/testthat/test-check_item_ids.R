test_that("check_item_ids returns NULL when item_ids is NULL", {
  ItemPool <- data.frame(item_id = c("A1", "A2", "A3"))
  expect_null(check_item_ids(NULL, item_names = c("A1", "A2", "A3")))
})

test_that("check_item_ids works with valid numeric indices", {
  ItemPool <- data.frame(item_id = c("A1", "A2", "A3"))
  # Should not error
  expect_silent(check_item_ids(1:3, item_names = c("A1", "A2", "A3")))
})

test_that("check_item_ids errors for numeric indices out of range", {
  ItemPool <- data.frame(item_id = c("A1", "A2", "A3"))

  expect_error(
    check_item_ids(c(0, 2), item_names = c("A1", "A2", "A3")),
    "'item_ids' must be valid indices within the item pool."
  )

  expect_error(
    check_item_ids(c(1, 5), item_names = c("A1", "A2", "A3")),
    "'item_ids' must be valid indices within the item pool."
  )
})

test_that("check_item_ids works with valid character item_ids", {
  ItemPool <- data.frame(item_id = c("A1", "A2", "A3"))

  # Should not error
  expect_silent(check_item_ids(c("A1", "A3"), item_names = c("A1", "A2", "A3")))
})

test_that("check_item_ids errors when character item_ids not found", {
  ItemPool <- data.frame(item_id = c("A1", "A2", "A3"))

  expect_error(
    check_item_ids(c("A1", "B999"), item_names = c("A1", "A2", "A3")),
    "not found"
  )
})

test_that("check_item_ids errors for unsupported item_ids type", {
  ItemPool <- data.frame(item_id = c("A1", "A2", "A3"))

  expect_error(
    check_item_ids(list(1, 2), item_names = c("A1", "A2", "A3")),
    "either numeric indices or character names"
  )
})
