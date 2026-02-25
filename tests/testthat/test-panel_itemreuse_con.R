x <- make_test_mstATA_BU()

test_that("panel_itemreuse_con errors for non-mstATA_design input", {

  expect_error(
    panel_itemreuse_con(x = list()),
    "Input x must be an object of class 'mstATA_design'",
    fixed = TRUE
  )
})

test_that("panel_itemreuse_con (non-overlap) returns valid constraint", {

  x <- make_test_mstATA_BU()

  out <- panel_itemreuse_con(x)

  # Class check
  expect_true(inherits(out, "mstATA_constraint"))

  # Base MST quantities
  PoolSize <- nrow(x$ItemPool)
  NumModules <- x$NumModules
  num_decisions <- PoolSize * NumModules

  # Matrix columns = decision variables
  expect_equal(ncol(out$A_binary), num_decisions)

  # Operators all <=
  expect_true(all(out$operators == "<="))

  # RHS all 1
  expect_true(all(out$d == 1))

  # Column names match decisionvar_name
  expect_equal(colnames(out$A_binary), x$decisionvar_name)

  expect_equal(Matrix::rowSums(out$A_binary),rep(NumModules,PoolSize))

  # Number of rows is the number of items with >1 occurrence across modules
  expect_equal(nrow(out$A_binary), length(out$name))

  for (i in 1:PoolSize) {
    expect_equal(as.vector(out$A_binary[i, (i+PoolSize*(seq_len(NumModules)-1L))]), rep(1,NumModules))
  }
})


test_that("panel_itemreuse_con (overlap = TRUE) returns valid constraint", {

  x <- make_test_mstATA_BU()

  out <- panel_itemreuse_con(x, overlap = TRUE)

  expect_true(inherits(out, "mstATA_constraint"))

  PoolSize <- nrow(x$ItemPool)
  NumModules <- x$NumModules
  NumStages<-x$NumStages
  NumPathways <- x$NumPathways
  num_decisions <- PoolSize * NumModules

  # Column count
  expect_equal(ncol(out$A_binary), num_decisions)

  # Operators all <=
  expect_true(all(out$operators == "<="))

  # RHS all 1
  expect_true(all(out$d == 1))

  # Correct names
  expect_equal(colnames(out$A_binary), x$decisionvar_name)

  expect_equal(Matrix::rowSums(out$A_binary),rep(NumStages,PoolSize*NumPathways))

  # Rows equal to length of filtered rows
  expect_equal(nrow(out$A_binary), length(out$name))

})
