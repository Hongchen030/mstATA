test_that("test_itemquant_con errors for non-mstATA_design input", {
  expect_error(
    test_itemquant_con(
      x = list(),
      attribute = "a_numeric",
      operator = ">=",
      target_value = 10
    ),
    "must be an object of class 'mstATA_design'",
    fixed = TRUE
  )
})

test_that("test_itemquant_con (module scope) builds a valid constraint", {

  x <- make_test_mstATA_BU()

  # assume test_itempool has a numeric attribute, e.g. "difficulty"
  attr <- names(x$ItemPool)[sapply(x$ItemPool, is.numeric)][1]

  out <- test_itemquant_con(
    x = x,
    attribute = attr,
    operator = ">=",
    target_value = 10,
    which_module = 1:2
  )

  expect_true(inherits(out, "mstATA_constraint"))

  PoolSize <- nrow(x$ItemPool)
  NumModules <- x$NumModules
  num_decisions <- PoolSize * NumModules

  expect_equal(ncol(out$A_binary), num_decisions)

  # correct number of rows
  # #modules × 1 quantitative constraint (one per module)
  expect_equal(nrow(out$A_binary), length(1:2))

  # operator repeated
  expect_equal(out$operators, rep(">=", length(1:2)))

  # RHS vector
  expect_equal(out$d, rep(10, length(1:2)))
})

test_that("test_itemquant_con (pathway scope) builds valid constraint", {

  x <- make_test_mstATA_BU()
  attr <- names(x$ItemPool)[sapply(x$ItemPool, is.numeric)][1]

  out <- test_itemquant_con(
    x = x,
    attribute = attr,
    operator = "<=",
    target_value = 30,
    which_pathway = 1
  )

  expect_true(inherits(out, "mstATA_constraint"))

  # one pathway → 1 constraint row
  expect_equal(nrow(out$A_binary), 1)

  # operator
  expect_equal(out$operators, "<=")

  # RHS
  expect_equal(out$d, 30)
})

test_that("invalid operator produces correct error", {

  x <- make_test_mstATA_BU()
  attr <- names(x$ItemPool)[sapply(x$ItemPool, is.numeric)][1]

  expect_error(
    test_itemquant_con(
      x = x, attribute = attr,
      operator = "??",
      target_value = 5
    )
  )
})

test_that("matrix column names match decisionvar_name", {

  x <- make_test_mstATA_BU()
  attr <- names(x$ItemPool)[sapply(x$ItemPool, is.numeric)][1]

  out <- test_itemquant_con(
    x = x,
    attribute = attr,
    operator = ">=",
    target_value = 5
  )

  expect_equal(colnames(out$A_binary), x$decisionvar_name)
})

test_that("target_value expands properly when vector is supplied", {

  x <- make_test_mstATA_BU()
  attr <- names(x$ItemPool)[sapply(x$ItemPool, is.numeric)][1]

  out <- test_itemquant_con(
    x = x,
    attribute = attr,
    operator = "<=",
    target_value = c(10, 20),     # two modules
    which_module = 1:2
  )

  expect_equal(out$d, c(10, 20))
})
test_that("non-numeric attribute produces informative error", {

  x <- make_test_mstATA_BU()

  # intentionally choose a categorical attribute
  attr <- names(x$ItemPool)[sapply(x$ItemPool, is.character)][1]

  expect_error(
    test_itemquant_con(
      x = x,
      attribute = attr,
      operator = ">=",
      target_value = 10
    ),
    "cannot be safely coerced to numeric",
  )
})


