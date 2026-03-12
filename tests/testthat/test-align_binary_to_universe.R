test_that("align_binary_to_universe aligns columns to full universe", {

  A <- Matrix::Matrix(
    c(1, 0,
      0, 1),
    nrow = 2,
    sparse = TRUE,
    dimnames = list(NULL, c("x1", "x3"))
  )

  dv_universe <- c("x1", "x2", "x3")

  Ab <- align_binary_to_universe(A, dv_universe)

  expect_equal(colnames(Ab), dv_universe)
  expect_equal(nrow(Ab), 2)
  expect_equal(ncol(Ab), 3)

  ## original columns preserved
  expect_equal(Ab[, "x1"], A[, "x1"])
  expect_equal(Ab[, "x3"], A[, "x3"])

  ## missing column filled with zeros
  expect_true(all(Ab[, "x2"] == 0))
})


test_that("align_binary_to_universe returns input unchanged if already aligned", {

  A <- Matrix::Matrix(
    c(1, 0,
      0, 1),
    nrow = 2,
    sparse = TRUE,
    dimnames = list(NULL, c("x1", "x2"))
  )

  dv_universe <- c("x1", "x2")

  Ab <- align_binary_to_universe(A, dv_universe)

  ## identical object returned
  expect_identical(Ab, A)
})


test_that("align_binary_to_universe coerces dense matrix to sparse", {

  A_dense <- matrix(
    c(1, 0,
      0, 1),
    nrow = 2,
    dimnames = list(NULL, c("x1", "x2"))
  )

  dv_universe <- c("x1", "x2", "x3")

  Ab <- align_binary_to_universe(A_dense, dv_universe)

  expect_s4_class(Ab, "sparseMatrix")
  expect_equal(colnames(Ab), dv_universe)
})


test_that("align_binary_to_universe errors on unknown variables", {

  A <- Matrix::Matrix(
    1,
    nrow = 1,
    sparse = TRUE,
    dimnames = list(NULL, "x_unknown")
  )

  dv_universe <- c("x1", "x2")

  expect_error(
    align_binary_to_universe(A, dv_universe),
    "A_binary contains variables not in dv_universe"
  )
})



test_that("align_binary_to_universe preserves row order and values", {

  A <- Matrix::Matrix(
    c(1, 0,
      0, 1,
      1, 1),
    nrow = 3,
    sparse = TRUE,
    dimnames = list(NULL, c("x2", "x1"))
  )

  dv_universe <- c("x1", "x2", "x3")

  Ab <- align_binary_to_universe(A, dv_universe)

  expect_equal(Ab[, "x1"], A[, "x1"])
  expect_equal(Ab[, "x2"], A[, "x2"])
  expect_true(all(Ab[, "x3"] == 0))
})

