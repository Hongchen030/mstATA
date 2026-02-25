test_that("itemcat_con errors when x is not mstATAdesign", {
  expect_error(
    itemcat_con(
      x = list(),
      item_ids = 1,
      select = TRUE,
      which_module = 1
    ),
    "must be an object of class 'mstATA_design'"
  )
})

test_that("itemcat_con errors for invalid character item_ids", {

  itempool <- data.frame(
    item_id = c("A1","A2","A3"),
    content = c("X","Y","X")
  )

  x <- mst_design(itempool, design = "1-3", pathway_length = 10)

  expect_error(
    itemcat_con(
      x,
      item_ids = c("A1","BAD"),
      select = TRUE,
      which_module = 1
    ),
    "Some item_ids not found"
  )
})

test_that("itemcat_con errors for invalid numeric item_ids", {

  itempool <- data.frame(
    item_id = 1:5,
    content = c("X","Y","X","Y","X")
  )

  x <- mst_design(itempool, design = "1-3", pathway_length = 10)

  expect_error(
    itemcat_con(
      x,
      item_ids = c(1, 10),
      select = TRUE,
      which_module = 1
    ),
    "must be valid indices"
  )
})

test_that("itemcat_con errors when both module and pathway specified", {

  itempool <- data.frame(item_id = 1:3)
  x <- mst_design(itempool, design = "1-3", pathway_length = 10)

  expect_error(
    itemcat_con(
      x,
      item_ids = 1,
      select = TRUE,
      which_module = 1,
      which_pathway = 1
    ),
    "either modules or pathways, not both"
  )
})

test_that("itemcat_con detects conflicts with decisionvar_name", {

  itempool <- data.frame(item_id = 1:3)
  x <- mst_design(itempool, design = "1-3", pathway_length = 10)

  # Make a fake eligibility filter
  x$decisionvar_name <- x$decisionvar_name[-1]

  expect_error(
    itemcat_con(
      x,
      item_ids = 1,
      select = TRUE,
      which_module = 1
    ),
    "Conflict with item_module_eligibility"
  )
})

test_that("itemcat_con builds correct MODULE-scope constraint", {

  itempool <- data.frame(item_id = 1:3)
  x <- mst_design(itempool, design = "1-3", pathway_length = 10)

  res <- itemcat_con(
    x,
    item_ids = 2,
    select = TRUE,
    which_module = 1
  )

  # One constraint
  expect_equal(nrow(res$A_binary), 1)

  # Should select → rhs = 1
  expect_equal(res$d, 1)

  # Coefficient for item 2 in module 1 should be 1
  pos <- which(colnames(res$A_binary) == "x[1,2]")
  expect_equal(as.vector(res$A_binary[1, pos]), 1)

  # Operators
  expect_equal(res$operators, "=")

  # Specification
  expect_equal(res$specification[["Application Level"]], "Module-level")
})

test_that("itemcat_con builds correct PATHWAY-scope constraint", {

  itempool <- data.frame(item_id = 1:6)
  x <- mst_design(itempool, design = "1-3-3", pathway_length = 10)

  res <- itemcat_con(
    x,
    item_ids = c(2, 3),
    select = TRUE,
    which_pathway = 1
  )

  NumStages   <- x$NumStages
  NumPathways <- x$NumPathways

  # n_items * n_pathways constraints
  expect_equal(nrow(res$A_binary), 2)

  # RHS
  expect_true(all(res$d == 1))

  # Check operator
  expect_true(all(res$operators == "="))

  # Should assign coefficients only to modules in pathway 1
  modules_in_pathway <- as.vector(as.matrix(
    x$PathwayIndex[x$PathwayIndex$pathway_index == 1, 1:NumStages]
  ))

  allowed_cols <- paste0("x[", rep(modules_in_pathway, each = 2), ",", rep(c(2,3), length(modules_in_pathway)), "]")


  active_cols <- colnames(res$A_binary)[
    which(res$A_binary[1,] != 0 | res$A_binary[2,] != 0)
  ]

  expect_true(all(active_cols %in% allowed_cols))
})

test_that("itemcat_con builds correct PANEL-scope constraint", {

  itempool <- data.frame(item_id = 1:3)
  x <- mst_design(itempool, design = "1-3", pathway_length = 10)

  res <- itemcat_con(
    x,
    item_ids = c(1,3),
    select = FALSE     # OMIT
  )

  # One constraint per item_id
  expect_equal(nrow(res$A_binary), 2)

  # Should omit → rhs = 0
  expect_true(all(res$d == 0))

  # Non-zero coefficients correspond to all modules
  expect_equal(
    sum(res$A_binary != 0),
    length(c(1,3)) * x$NumModules
  )

})
