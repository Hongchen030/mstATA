test_that("check_target_bounds computes min/max from target ± deviation", {

  levels <- c("A", "B")
  total_rows <- 2
  row_ids <- 1:2

  out <- check_target_bounds(
    levels = levels,
    target = c(10, 20),
    deviation = c(2, 3),
    total_rows = total_rows,
    row_ids = row_ids
  )

  expect_equal(out$min, matrix(c(8, 8,17,17), nrow = 2, byrow = FALSE))
  expect_equal(out$max, matrix(c(12,12,23,23), nrow = 2, byrow = FALSE))
})

test_that("check_target_bounds errors when neither min/max nor target/deviation is complete", {

  levels <- "A"
  total_rows <- 2
  row_ids <- 1:2

  expect_error(
    check_target_bounds(levels, min = NULL, max = 5,target = 3,deviation = NULL,
                        total_rows = total_rows, row_ids = row_ids),
    "You must either provide 'min' and 'max', or 'target' and 'deviation'.",
    fixed = TRUE
  )

  expect_error(
    check_target_bounds(levels, min = NULL, max = 5,
                        total_rows = total_rows, row_ids = row_ids),
    "You must either provide 'min' and 'max', or 'target' and 'deviation'.",
    fixed = TRUE
  )

  expect_error(
    check_target_bounds(levels, min = 1, max = NULL,
                        total_rows = total_rows, row_ids = row_ids),
    "You must either provide 'min' and 'max', or 'target' and 'deviation'.",
    fixed = TRUE
  )
})

test_that("check_target_bounds expands scalar min/max correctly", {

  levels <- c("A", "B")
  total_rows <- 3
  row_ids <- 1:3

  out <- check_target_bounds(
    levels = levels,
    min = 1,
    max = 5,
    total_rows = total_rows,
    row_ids = row_ids
  )

  expect_equal(out$min, matrix(1, nrow = 3, ncol = 2))
  expect_equal(out$max, matrix(5, nrow = 3, ncol = 2))
})

test_that("check_target_bounds expands vector min/max correctly", {

  levels <- c("A", "B", "C")
  total_rows <- 2
  row_ids <- 1:2

  out <- check_target_bounds(
    levels = levels,
    min = c(1, 2, 3),
    max = c(4, 5, 6),
    total_rows = total_rows,
    row_ids = row_ids
  )

  expect_equal(out$min,
               matrix(rep(c(1,2,3), each = 2), nrow = 2, byrow = FALSE))

  expect_equal(out$max,
               matrix(rep(c(4,5,6), each = 2), nrow = 2, byrow = FALSE))
})

test_that("check_target_bounds returns matrices unchanged if they already are matrices", {

  levels <- c("A", "B")
  total_rows <- 2
  row_ids <- 1:2

  min_mat <- matrix(c(1, 2, 3, 4), nrow = 2)
  max_mat <- matrix(c(5, 6, 7, 8), nrow = 2)

  out <- check_target_bounds(
    levels = levels,
    min = min_mat,
    max = max_mat,
    total_rows = total_rows,
    row_ids = row_ids
  )

  expect_identical(out$min, min_mat)
  expect_identical(out$max, max_mat)
})

test_that("check_target_bounds expands target and deviation for multiple levels", {

  levels <- c("L1", "L2", "L3")
  total_rows <- 2
  row_ids <- 1:2

  out <- check_target_bounds(
    levels = levels,
    target = 10,
    deviation = 2,
    total_rows = total_rows,
    row_ids = row_ids
  )

  expect_equal(out$min, matrix(8, nrow = 2, ncol = 3))
  expect_equal(out$max, matrix(12, nrow = 2, ncol = 3))
})
