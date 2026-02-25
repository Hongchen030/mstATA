test_that("expand_A_real_parallel replicates rows but not columns", {
  A_real <- Matrix::Matrix(
    c(1, 2,
      3, 4),
    nrow = 2,
    sparse = TRUE,
    dimnames = list(NULL, c("y1", "y2"))
  )

  A_mp <- expand_A_real_parallel(A_real, 3)

  expect_equal(dim(A_mp), c(6, 2))
  expect_equal(colnames(A_mp), c("y1", "y2"))

  expect_equal(
    A_mp,
    rbind(
      A_real,
      A_real,
      A_real
    )
  )
})

test_that("expand_A_real_parallel returns NULL when input is NULL", {
  expect_null(expand_A_real_parallel(NULL, 3))
})


test_that("expand_A_real_parallel returns input for single panel", {
  A_real <- Matrix::Matrix(
    c(1, -1),
    nrow = 2,
    sparse = TRUE,
    dimnames = list(NULL, "y")
  )

  out <- expand_A_real_parallel(A_real, 1)

  expect_identical(out, A_real)
})


test_that("expand_A_real_parallel preserves sparsity", {
  A_real <- Matrix::Matrix(
    c(0, 1,
      0, 0),
    nrow = 2,
    sparse = TRUE)

  out <- expand_A_real_parallel(A_real, 4)

  expect_true(inherits(out, "sparseMatrix"))
  expect_equal(nrow(out), 8)
})

