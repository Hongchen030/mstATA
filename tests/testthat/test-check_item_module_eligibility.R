test_that("returns full eligibility when input is NULL", {
  index_map <- setNames(1:5, paste0("i", 1:5))

  out <- check_item_module_eligibility(
    item_module_eligibility = NULL,
    index_map = index_map,
    NumModules = 3
  )

  expect_type(out, "list")
  expect_length(out, 3)
  expect_named(out, c("1", "2", "3"))

  for (m in 1:3) {
    expect_equal(out[[as.character(m)]], 1:5)
  }
})


test_that("errors if module names are missing or non-numeric", {
  index_map <- setNames(1:3, c("a", "b", "c"))

  expect_error(
    check_item_module_eligibility(
      list(1:3),
      index_map = index_map,
      NumModules = 2
    ),
    "named list"
  )

  expect_error(
    check_item_module_eligibility(
      list(a = 1:3),
      index_map = index_map,
      NumModules = 2
    ),
    "module"
  )
})


test_that("errors if module indices are out of range", {
  index_map <- setNames(1:3, c("a", "b", "c"))

  expect_error(
    check_item_module_eligibility(
      list(`0` = 1:2),
      index_map = index_map,
      NumModules = 3
    ),
    "between 1 and"
  )

  expect_error(
    check_item_module_eligibility(
      list(`4` = 1:2),
      index_map = index_map,
      NumModules = 3
    ),
    "between 1 and"
  )
})


test_that("accepts numeric item indices and validates bounds", {
  index_map <- setNames(1:4, c("i1", "i2", "i3", "i4"))

  out <- check_item_module_eligibility(
    list(`1` = c(1, 3, 3)),
    index_map = index_map,
    NumModules = 2
  )

  expect_equal(out[["1"]], c(1, 3))
  expect_equal(out[["2"]], 1:4)
})


test_that("errors on invalid numeric item indices", {
  index_map <- setNames(1:4, c("i1", "i2", "i3", "i4"))

  expect_error(
    check_item_module_eligibility(
      list(`1` = c(0, 2)),
      index_map = index_map,
      NumModules = 2
    ),
    "item"
  )

  expect_error(
    check_item_module_eligibility(
      list(`1` = c(2, 5)),
      index_map = index_map,
      NumModules = 2
    ),
    "item"
  )
})


test_that("accepts character item names via index_map", {
  index_map <- setNames(1:3, c("A", "B", "C"))

  out <- check_item_module_eligibility(
    list(`2` = c("A", "C")),
    index_map = index_map,
    NumModules = 3
  )

  expect_equal(out[["2"]], c(1, 3))
  expect_equal(out[["1"]], 1:3)
  expect_equal(out[["3"]], 1:3)
})


test_that("errors on unknown item names", {
  index_map <- setNames(1:3, c("A", "B", "C"))

  expect_error(
    check_item_module_eligibility(
      list(`1` = c("A", "D")),
      index_map = index_map,
      NumModules = 2
    ),
    "Unknown item"
  )
})


test_that("errors if a module has an empty eligibility set", {
  index_map <- setNames(1:3, c("A", "B", "C"))

  expect_error(
    check_item_module_eligibility(
      list(`1` = integer(0)),
      index_map = index_map,
      NumModules = 2
    ),
    "empty"
  )
})


test_that("output is always normalized and solver-safe", {
  index_map <- setNames(1:5, paste0("i", 1:5))

  out <- check_item_module_eligibility(
    list(
      `3` = c("i2", "i4"),
      `1` = c(5, 1)
    ),
    index_map = index_map,
    NumModules = 3
  )

  expect_named(out, c("1", "2", "3"))
  expect_true(all(vapply(out, is.integer, logical(1))))
  expect_true(all(vapply(out, function(x) all(x >= 1 & x <= 5), logical(1))))
})


