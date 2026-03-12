test_that("test_stimquant_con errors when x is not mstATA_design", {
  expect_error(
    test_stimquant_con(
      x = list(),
      attribute = "stimulus_words",
      operator = ">=",
      target_value = 100,
      which_module = 1
    ),
    "must be an object of class 'mstATA_design'"
  )
})

test_that("test_stimquant_con errors when operator is invalid", {
  expect_error(
    test_stimquant_con(
      x = make_test_mstATA_BU(),
      attribute = "time",
      operator = ">>>",
      target_value = 100,
      which_module = 1
    ),
    "Invalid operator"
  )
})

test_that("test_stimquant_con errors when pivot has no quantitative value", {
  test_itempool[which(test_itempool$pivot_item=="Y")[1],"stimulus_words"]<-NA_real_
  pivot_stim_map=create_pivot_stimulus_map(itempool = test_itempool,item_id_col = "item_id",stimulus = "stimulus",pivot_item = "pivot_item")
  x_bad<-mst_design(itempool = test_itempool,design = "1-3-3",pathway_length = 30,
                    pivot_stim_map = pivot_stim_map)
  expect_error(
    test_stimquant_con(
      x = x_bad,
      attribute = "stimulus_words",
      operator = ">=",
      target_value = 100,
      which_module = 1
    ),
    "Quantitative attribute: 'stimulus_words' must not contain NA, NaN, or infinite values."
  )
})

test_that("test_stimquant_con builds correct module-level constraint", {
  x <- make_test_mstATA_BU()

  res <- test_stimquant_con(
    x = x,
    attribute = "stimulus_words",
    operator = ">=",
    target_value = 100,
    which_module = 1
  )

  # One constraint per module in scope
  expect_equal(nrow(res$A_binary), length(1))  # which_module = 1
  expect_equal(ncol(res$A_binary), x$NumModules * nrow(x$ItemPool))

  # Operator and RHS
  expect_equal(res$operators, ">=")
  expect_equal(res$d, 100)

  # Specification
  spec <- res$specification
  expect_equal(spec$Attribute, "stimulus_words")
  expect_equal(spec$Type, "Quantitative")
  expect_equal(spec$`Application Level`, "Module-level")
  expect_equal(spec$`Num of Constraints`, 1)

  # Non-zero coefficients must be positive numbers
  nz <- Matrix::summary(res$A_binary)
  expect_true(all(nz$x >= 0))
})

test_that("test_stimquant_con builds correct pathway-level constraint", {
  x <- make_test_mstATA_BU()

  res <- test_stimquant_con(
    x = x,
    attribute = "stimulus_words",
    operator = "<=",
    target_value = 200,
    which_pathway = 1
  )

  # One constraint per pathway in scope
  expect_equal(nrow(res$A_binary), length(1))  # which_pathway = 1
  expect_equal(ncol(res$A_binary), x$NumModules * nrow(x$ItemPool))

  expect_equal(res$operators, "<=")
  expect_equal(res$d, 200)

  spec <- res$specification
  expect_equal(spec$`Application Level`, "Pathway-level")
  expect_equal(spec$Type, "Quantitative")
})

test_that("test_stimquant_con respects decisionvar_name ordering", {
  x <- make_test_mstATA_BU()

  # fabricate a custom decisionvar_name (for example, subset or re-ordered)
  # For a realistic test, you might use something like:
  # x$decisionvar_name <- sample(colnames_of_decision_vars)
  # Here we just reuse existing:
  x$decisionvar_name <- x$decisionvar_name

  res <- test_stimquant_con(
    x = x,
    attribute = "stimulus_words",
    operator = "=",
    target_value = 50,
    which_module = 1
  )

  expect_true(all(colnames(res$A_binary)==x$decisionvar_name))
})
