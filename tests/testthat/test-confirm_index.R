test_that("valid integer index returns TRUE", {
  expect_true(confirm_index(
    index = as.integer(c(1, 3, 5)),
    pool_size = 5
  ))
})

test_that("single integer index is valid", {
  expect_true(confirm_index(
    index = as.integer(2),
    pool_size = 5
  ))
})

test_that("non-integer index triggers error", {
  expect_error(
    confirm_index(
      index = c(1, 2, 3),  # numeric, not integer
      pool_size = 5
    ),
    "index must be an integer vector"
  )
})

test_that("NA values trigger error", {
  expect_error(
    confirm_index(
      index = as.integer(c(1, NA, 3)),
      pool_size = 5
    ),
    "index contains NA values"
  )
})

test_that("out-of-range values trigger error", {
  expect_error(
    confirm_index(
      index = as.integer(c(0, 2)),
      pool_size = 5
    ),
    "index contains values outside \\[1, 5\\]"
  )

  expect_error(
    confirm_index(
      index = as.integer(c(1, 6)),
      pool_size = 5
    ),
    "index contains values outside \\[1, 5\\]"
  )
})

test_that("custom 'what' is reflected in error message", {
  expect_error(
    confirm_index(
      index = c(1, 2),
      pool_size = 5,
      what = "module index"
    ),
    "module index must be an integer vector"
  )
})

test_that("list of valid indices is allowed by default", {
  idx_list <- list(
    as.integer(c(1, 2)),
    as.integer(3),
    as.integer(c(4, 5))
  )

  expect_true(confirm_index(
    index = idx_list,
    pool_size = 5
  ))
})

test_that("list input fails when allow_list = FALSE", {
  idx_list <- list(
    as.integer(c(1, 2)),
    as.integer(3)
  )

  expect_error(
    confirm_index(
      index = idx_list,
      pool_size = 5,
      allow_list = FALSE
    ),
    "index must be an integer vector"
  )
})

test_that("list containing invalid index triggers error", {
  idx_list <- list(
    as.integer(c(1, 2)),
    as.integer(c(0, 3))  # invalid
  )

  expect_error(
    confirm_index(
      index = idx_list,
      pool_size = 5
    ),
    "index contains values outside \\[1, 5\\]"
  )
})
