test_that("test_stimcount_con errors when x is not mstATA_design", {
  expect_error(
    test_stimcount_con(
      x = list(),
      target_num = 2,
      operator = ">=",
      which_module = 1
    ),
    "must be an object of class 'mstATA_design'"
  )
})

test_that("test_stimcount_con errors when operator is invalid", {
  expect_error(
    test_stimcount_con(
      x = make_test_mstATA_BU(),
      target_num = 2,
      operator = ">>>",
      which_module = 1
    ),
    "Must be one of"
  )
})


test_that("test_stimcount_con creates valid module-level constraint", {
  res <- test_stimcount_con(
    x = make_test_mstATA_BU(),
     target_num = 3,
    operator = ">=",
    which_module = 1
  )

  # Expect correct operator
  expect_equal(res$operators, ">=")

  # Expect one constraint row
  expect_equal(nrow(res$A_binary), 1)

  # RHS correct
  expect_equal(res$d, 3)


  # Only pivot items contribute (binary 1s)
  expect_true(any(res$A_binary[1, ] == 1))
})

test_that("test_stimcount_con creates valid pathway-level constraint", {
  res <- test_stimcount_con(
    x = make_test_mstATA_TD(),
     target_num = 1,
    operator = "<=",
    which_pathway = 1
  )

  expect_equal(res$operators, "<=")
  expect_equal(nrow(res$A_binary), 1)
  expect_equal(res$d, 1)

  # Pathway-level constraint should sum pivot items across all its modules
  expect_true(sum(res$A_binary[1, ]) >= 1)
})


