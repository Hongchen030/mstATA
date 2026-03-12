test_that("error when both which_module and which_pathway are specified", {
  expect_error(
    check_test_constraint_scope(
      num_modules = 3,
      num_pathways = 2,
      which_module = 1,
      which_pathway = 1
    ),
    "Specify either"
  )
})

test_that("module-level scope when which_module is specified", {
  out <- check_test_constraint_scope(
    num_modules = 4,
    num_pathways = 3,
    which_module = c(1, 3)
  )

  expect_type(out, "list")
  expect_equal(out$application_level, "Module-level")
  expect_equal(out$which_module, c(1, 3))
  expect_null(out$which_pathway)
  expect_equal(out$total_rows, 4)
  expect_equal(out$row_ids, c(1, 3))
})

test_that("pathway-level scope when which_pathway is specified", {
  out <- check_test_constraint_scope(
    num_modules = 5,
    num_pathways = 4,
    which_pathway = c(2, 4)
  )

  expect_type(out, "list")
  expect_equal(out$application_level, "Pathway-level")
  expect_null(out$which_module)
  expect_equal(out$which_pathway, c(2, 4))
  expect_equal(out$total_rows, 4)
  expect_equal(out$row_ids, c(2, 4))
})

test_that("default is module-level when neither which_module nor which_pathway is specified", {
  out <- check_test_constraint_scope(
    num_modules = 3,
    num_pathways = 2
  )

  expect_equal(out$application_level, "Module-level")
  expect_null(out$which_pathway)
  expect_equal(out$total_rows, 3)

  # depends on validate_module_selection(NULL, num_modules)
  # usually returns 1:num_modules
  expect_equal(out$row_ids, 1:3)
  expect_equal(out$which_module, 1:3)
})

test_that("validate_module_selection is enforced", {
  expect_error(
    check_test_constraint_scope(
      num_modules = 3,
      num_pathways = 2,
      which_module = 5
    )
  )
})

test_that("validate_pathway_selection is enforced", {
  expect_error(
    check_test_constraint_scope(
      num_modules = 3,
      num_pathways = 2,
      which_pathway = 0
    )
  )
})
