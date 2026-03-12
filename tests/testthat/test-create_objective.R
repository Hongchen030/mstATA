test_that("create_objective constructs a valid compiled_objective", {

  A_bin  <- Matrix::sparseMatrix(i = 1:3, j = 1:3, x = 1)
  A_real <- Matrix::sparseMatrix(i = 1:3, j = c(1,1,1), x = c(-1,0,0))

  obj <- create_objective(
    name = NULL,
    specification = NULL,
    A_binary = A_bin,
    A_real = A_real,
    C_binary = NULL,C_real = 1,
    operator = rep("<=", 3),
    d = c(0,2,3),
    sense = "max",
    decisionvar_name_new = "z1",
    decisionvar_type_new = "C"
  )

  expect_s3_class(obj, "compiled_objective")

  # Structure checks
  expect_equal(obj$sense, "max")
  expect_equal(nrow(obj$A_binary), 3)
  expect_equal(nrow(obj$A_real), 3)
  expect_equal(length(obj$operators), 3)
  expect_equal(length(obj$d), 3)
  expect_equal(length(obj$decisionvar_name_new), 1)
  expect_equal(length(obj$decisionvar_type_new), 1)
})


library(Matrix)

make_valid_matrices <- function(n_rows = 2, n_bin = 3, n_real = 1) {
  list(
    A_binary = Matrix(0, nrow = n_rows, ncol = n_bin, sparse = TRUE),
    A_real   = Matrix(0, nrow = n_rows, ncol = n_real, sparse = TRUE)
  )
}

test_that("create_objective returns compiled_objective for valid inputs", {
  mats <- make_valid_matrices()

  obj <- create_objective(
    A_binary = mats$A_binary,
    A_real   = mats$A_real,
    C_binary = rep(0, ncol(mats$A_binary)),
    C_real   = rep(0, ncol(mats$A_real)),
    operator = c("<=", "<="),
    d = c(1, 2),
    sense = "max",
    decisionvar_name_new = "delta",
    decisionvar_type_new = "continuous"
  )

  expect_s3_class(obj, "compiled_objective")
  expect_equal(obj$sense, "max")
  expect_equal(ncol(obj$A_binary), 3)
  expect_equal(ncol(obj$A_real), 1)
})

test_that("invalid sense triggers error", {
  mats <- make_valid_matrices()

  expect_error(
    create_objective(
      A_binary = mats$A_binary,
      A_real   = mats$A_real,
      operator = c("<=", "<="),
      d = c(1, 2),
      sense = "maximize",
      decisionvar_name_new = "delta",
      decisionvar_type_new = "continuous"
    )
  )
})

test_that("A_binary must be a sparse Matrix", {
  mats <- make_valid_matrices()

  expect_error(
    create_objective(
      A_binary = matrix(0, 2, 3),
      A_real   = mats$A_real,
      operator = c("<=", "<="),
      d = c(1, 2),
      sense = "min",
      decisionvar_name_new = "delta",
      decisionvar_type_new = "continuous"
    ),
    "A_binary must be a sparse Matrix"
  )
})

test_that("A_real must be non-NULL and a sparse Matrix", {
  mats <- make_valid_matrices()

  expect_error(
    create_objective(
      A_binary = mats$A_binary,
      A_real   = NULL,
      operator = c("<=", "<="),
      d = c(1, 2),
      sense = "min",
      decisionvar_name_new = "delta",
      decisionvar_type_new = "continuous"
    ),
    "A_real must be non-NULL"
  )

  expect_error(
    create_objective(
      A_binary = mats$A_binary,
      A_real   = matrix(0, 2, 1),
      operator = c("<=", "<="),
      d = c(1, 2),
      sense = "min",
      decisionvar_name_new = "delta",
      decisionvar_type_new = "continuous"
    ),
    "A_real must be a sparse Matrix"
  )
})

test_that("operator length must match number of rows", {
  mats <- make_valid_matrices(n_rows = 2)

  expect_error(
    create_objective(
      A_binary = mats$A_binary,
      A_real   = mats$A_real,
      C_real = 1,
      operator = "<=",
      d = 1,
      sense = "max",
      decisionvar_name_new = "delta",
      decisionvar_type_new = "continuous"
    ),
    "operator has 1"
  )
})

test_that("row mismatch between A_binary and A_real triggers error", {
  A_binary <- Matrix(0, nrow = 2, ncol = 3, sparse = TRUE)
  A_real   <- Matrix(0, nrow = 3, ncol = 1, sparse = TRUE)

  expect_error(
    create_objective(
      A_binary = A_binary,
      A_real   = A_real,
      C_real = 1,
      operator = c("<=", "<="),
      d = c(1, 2),
      sense = "min",
      decisionvar_name_new = "delta",
      decisionvar_type_new = "continuous"
    ),
    "Mismatch"
  )
})


test_that("decisionvar_name_new length must match ncol(A_real)", {
  mats <- make_valid_matrices(n_real = 2)

  expect_error(
    create_objective(
      A_binary = mats$A_binary,
      A_real   = mats$A_real,
      operator = c("<=", "<="),
      C_real = c(1,1),
      d = c(1, 2),
      sense = "max",
      decisionvar_name_new = "delta",
      decisionvar_type_new = "continuous"
    ),
    "A_real has 2 columns"
  )
})

test_that("C_binary and C_real default correctly", {
  mats <- make_valid_matrices()

  obj <- create_objective(
    A_binary = mats$A_binary,
    A_real   = mats$A_real,
    operator = c("<=", "<="),
    C_real = 1,
    d = c(1, 2),
    sense = "min",
    decisionvar_name_new = "delta",
    decisionvar_type_new = "continuous"
  )

  expect_equal(obj$C_binary, numeric(3))
  expect_equal(obj$C_real, 1)
})



