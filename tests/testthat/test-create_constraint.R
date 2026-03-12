test_that("create_constraint() works with binary-only matrix", {
  A <- Matrix::Matrix(c(1, 0, 0, 0, 1, 1), nrow = 2, sparse = TRUE)
  colnames(A)<-paste("decisionvar",1:3)
  result <- create_constraint(
    name = NULL,
    specification = NULL,
    A_binary = A,
    operators = c("<=", "="),
    d = c(2, 3))
  expect_s3_class(result, "mstATA_constraint")
  expect_equal(nrow(result$A_binary), 2)
  expect_equal(ncol(result$A_binary),3)
  expect_equal(result$operators,c("<=","="))
  expect_equal(result$d,c(2,3))
  expect_equal(result$A_real,NULL)
  expect_equal(result$C_binary,NULL)
  expect_equal(result$C_real,NULL)
  expect_equal(result$sense,NULL)
})

test_that("create_constraint() throws errors for invalid inputs", {
  A <- matrix(1, 2, 2)  # Not sparse
  expect_error(create_constraint(
    name = c("a", "b"),
    A_binary = A,
    operators = rep("<=", 2),
    d = c(1, 2)
  ), "A_binary must be a sparse Matrix")

  A_sparse <- Matrix::Matrix(1, 2, 2, sparse = TRUE)
  expect_error(create_constraint(
    name = c("a", "b"),
    A_binary = A_sparse,
    operators = rep("<=", 3),
    d = c(1, 2)
  ), "Length of 'd' and 'operators' must match the number of rows in A_binary.")

  expect_error(create_constraint(
    name = c("a", "b"),
    A_binary = A_sparse,
    A_real = matrix(-1,nrow = 3,ncol = 1),
    operators = rep("<=", 2),
    d = c(1, 2)
  ),"A_real must be NULL for mstATA_constraints")

})



