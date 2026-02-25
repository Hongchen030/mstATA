test_that("NULL which_module returns all modules", {
  out <- validate_module_selection(
    which_module = NULL,
    num_modules = 4
  )

  expect_equal(out, 1:4)
})

test_that("valid integer indices are returned sorted and unique", {
  out <- validate_module_selection(
    which_module = c(3, 1, 2, 2),
    num_modules = 4
  )

  expect_equal(out, c(1, 2, 3))
})

test_that("numeric whole numbers are accepted", {
  out <- validate_module_selection(
    which_module = c(1, 3, 4),
    num_modules = 5
  )

  expect_equal(out, c(1, 3, 4))
})

test_that("non-integer numeric values trigger error", {
  expect_error(
    validate_module_selection(
      which_module = c(1.5, 2),
      num_modules = 5
    ),
    "'which_module' must be a vector of integers"
  )
})

test_that("non-numeric input triggers error", {
  expect_error(
    validate_module_selection(
      which_module = c("1", "2"),
      num_modules = 5
    ),
    "'which_module' must be a vector of integers"
  )
})

test_that("out-of-range indices trigger error", {
  expect_error(
    validate_module_selection(
      which_module = c(0, 2),
      num_modules = 5
    ),
    "Invalid 'which_module' index"
  )

  expect_error(
    validate_module_selection(
      which_module = c(1, 6),
      num_modules = 5
    ),
    "Invalid 'which_module' index"
  )
})

test_that("negative indices trigger error", {
  expect_error(
    validate_module_selection(
      which_module = c(-1, 2),
      num_modules = 5
    ),
    "Invalid 'which_module' index"
  )
})

test_that("boundary values are accepted", {
  out <- validate_module_selection(
    which_module = c(1, 5),
    num_modules = 5
  )

  expect_equal(out, c(1, 5))
})
