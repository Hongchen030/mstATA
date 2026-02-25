test_that("Module-level scope validates correctly", {
  # Correct input: module-level with scalar module index
  out <- validate_scope(
    applied_level = "Module-level",
    which_module = 2,
    which_pathway = NULL,
    num_modules = 5,
    num_pathways = 3
  )

  expect_equal(out$which_module, 2)
  expect_null(out$which_pathway)

  # Missing which_module
  expect_error(
    validate_scope(
      applied_level = "Module-level",
      which_module = NULL,
      which_pathway = NULL,
      num_modules = 5,
      num_pathways = 3
    ),
    "requires 'which_module' to be provided"
  )

  # Non-scalar module index
  expect_error(
    validate_scope(
      applied_level = "Module-level",
      which_module = c(1, 2),
      which_pathway = NULL,
      num_modules = 5,
      num_pathways = 3
    ),
    "must be a scalar"
  )

  # which_pathway must be NULL
  expect_error(
    validate_scope(
      applied_level = "Module-level",
      which_module = 1,
      which_pathway = 2,
      num_modules = 5,
      num_pathways = 3
    ),
    "requires 'which_pathway' = NULL"
  )
})


test_that("Pathway-level scope validates correctly", {
  # Correct input: pathway-level with scalar index
  out <- validate_scope(
    applied_level = "Pathway-level",
    which_module = NULL,
    which_pathway = 1,
    num_modules = 5,
    num_pathways = 3
  )

  expect_equal(out$which_pathway, 1)
  expect_null(out$which_module)

  # Missing which_pathway
  expect_error(
    validate_scope(
      applied_level = "Pathway-level",
      which_module = NULL,
      which_pathway = NULL,
      num_modules = 5,
      num_pathways = 3
    ),
    "requires 'which_pathway' to be provided"
  )

  # Non-scalar pathway index
  expect_error(
    validate_scope(
      applied_level = "Pathway-level",
      which_module = NULL,
      which_pathway = c(1, 2),
      num_modules = 5,
      num_pathways = 3
    ),
    "must be a scalar"
  )

  # which_module must be NULL
  expect_error(
    validate_scope(
      applied_level = "Pathway-level",
      which_module = 2,
      which_pathway = 1,
      num_modules = 5,
      num_pathways = 3
    ),
    "requires 'which_module' = NULL"
  )
})


test_that("Panel-level scope validates correctly", {
  # Correct: no module/pathway index should be provided
  out <- validate_scope(
    applied_level = "Panel-level",
    which_module = NULL,
    which_pathway = NULL,
    num_modules = 5,
    num_pathways = 3
  )

  expect_null(out$which_module)
  expect_null(out$which_pathway)

  # Reject which_module
  expect_error(
    validate_scope(
      applied_level = "Panel-level",
      which_module = 1,
      which_pathway = NULL,
      num_modules = 5,
      num_pathways = 3
    ),
    "does not accept which_module or which_pathway"
  )

  # Reject which_pathway
  expect_error(
    validate_scope(
      applied_level = "Panel-level",
      which_module = NULL,
      which_pathway = 2,
      num_modules = 5,
      num_pathways = 3
    ),
    "does not accept which_module or which_pathway"
  )
})


test_that("Invalid applied_level is rejected", {
  expect_error(
    validate_scope(
      applied_level = "InvalidLevel",
      which_module = NULL,
      which_pathway = NULL,
      num_modules = 5,
      num_pathways = 3
    ),
    "arg"
  )
})
