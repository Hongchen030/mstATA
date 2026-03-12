test_that("expand_target_to_matrix handles matrix input of full size", {
  levels <- c("A", "B")
  total_rows <- 5
  row_ids <- c(2, 4)
  input <- matrix(1:10, nrow = 5, ncol = 2)

  out <- expand_target_to_matrix(input, levels, total_rows, row_ids)

  expect_equal(dim(out), c(2, 2))
  expect_equal(out, input[row_ids, ])
})

test_that("expand_target_to_matrixfor valid matrix size", {
  levels <- c("A", "B")
  total_rows <- 5
  row_ids <- 1:2
  input <- matrix(1:4, nrow = 2, ncol = 2)
  out <- expand_target_to_matrix(input, levels, total_rows, row_ids)
  expect_equal(out,input)
})

test_that("expand_target_to_matrix errors for invalid matrix size", {
  levels <- c("A", "B")
  total_rows <- 5
  row_ids <- 1:3
  input <- matrix(1:4, nrow = 2, ncol = 2)

  expect_error(
    expand_target_to_matrix(input, levels, total_rows, row_ids)
  )
})



test_that("scalar input expands correctly", {
  levels <- c("A", "B")
  row_ids <- 1:4
  total_rows <- 4
  input <- 3

  out <- expand_target_to_matrix(input, levels, total_rows, row_ids)

  expect_equal(dim(out), c(4, 2))
  expect_true(all(out == 3))
})

test_that("vector of length n_cols expands correctly", {
  levels <- c("L1", "L2", "L3")
  row_ids <- 1:4
  total_rows <- 4
  input <- c(1, 2, 3)

  out <- expand_target_to_matrix(input, levels, total_rows, row_ids)

  expect_equal(dim(out), c(4, 3))
  expect_equal(out[, 1], rep(1, 4))
  expect_equal(out[, 2], rep(2, 4))
  expect_equal(out[, 3], rep(3, 4))
})

test_that("vector of length n_rows expands correctly", {
  levels <- c("A", "B")
  row_ids <- 1:3
  total_rows <- 3
  input <- c(10, 20, 30)

  out <- expand_target_to_matrix(input, levels, total_rows, row_ids)

  expect_equal(dim(out), c(3, 2))
  expect_equal(out[1, ], c(10, 10))
  expect_equal(out[2, ], c(20, 20))
  expect_equal(out[3, ], c(30, 30))
})

test_that("vector of length total_rows and n_cols = 1 subsets correctly", {
  levels <- NULL
  total_rows <- 5
  row_ids <- c(1, 3, 5)
  input <- 1:5

  out <- expand_target_to_matrix(input, levels, total_rows, row_ids)

  expect_equal(dim(out), c(3, 1))
  expect_equal(out[, 1], c(1, 3, 5))
})

test_that("vector of length n_rows and n_cols = 1 returns as column", {
  levels <- 1  # n_cols = 1
  row_ids <- 1:4
  total_rows <- 4
  input <- c(7, 8, 9, 10)

  out <- expand_target_to_matrix(input, levels, total_rows, row_ids)

  expect_equal(dim(out), c(4, 1))
  expect_equal(out[, 1], input)
})

test_that("invalid input length triggers error", {
  levels <- c("A", "B")
  total_rows <- 5
  row_ids <- 1:3
  input <- c(1, 2, 3, 4)  # does not match any valid case

  expect_error(
    expand_target_to_matrix(input, levels, total_rows, row_ids),
    "Input cannot be expanded into the expected \\(n_rows x n_cols\\) matrix\\."
  )
})

