test_that("objective_term errors on wrong x input", {
  expect_error(
    objective_term(x = list(), attribute = "difficulty"),
    "Input 'x' must be an object of class 'mstATA_design'"
  )
})

test_that("objective_term handles relative objective correctly", {
  x <- mst_design(itempool = mini_itempool,design = "1-3",pathway_length = 8)

  obj <- objective_term(
    x = x,
    attribute = "difficulty",
    cat_level = NULL,
    applied_level = "Module-level",
    which_module = 1,
    sense = "max"
  )

  expect_s3_class(obj, "mstATA_objective")
  expect_true(grepl("Relative Objective: Maximize", obj$name))
  expect_true(grepl("the sum of difficulty values", obj$name))
  expect_equal(obj$sense, "max")
  expect_equal(sum(obj$coef_val),sum(mini_itempool$difficulty))
  expect_true(is.null(obj$goal),TRUE)
})

test_that("absolute objective coerces sense to min and shows message", {
  x <- mst_design(itempool = mini_itempool,design = "1-3",pathway_length = 8)

  expect_message(
    obj <- objective_term(
      x = x,
      attribute = "difficulty",
      cat_level = NULL,
      applied_level = "Module-level",
      which_module = 1,
      sense = "max",
      goal = 10
    ),
    "Numeric target supplied; switching sense to 'min' for absolute deviation objective."
  )

  expect_equal(obj$sense, "min")
  expect_true(grepl("Absolute Objective: Minimize the absolute deviation", obj$name))
  expect_equal(obj$goal,10)
})

test_that("objective_term errors on invalid goal", {
  x <- mst_design(itempool = mini_itempool,design = "1-3",pathway_length = 8)

  expect_error(
    objective_term(
      x = x, attribute = "difficulty", applied_level = "Module-level",
      which_module = 1, goal = c(1,2)
    ),
    "Invalid 'goal'"
  )
})

test_that("module scope builds correct matrix size", {
  x <- mst_design(itempool = mini_itempool,design = "1-3",pathway_length = 8)

  obj <- objective_term(
    x = x,
    attribute = "difficulty",
    applied_level = "Module-level",
    which_module = 1
  )

  mat <- obj$coef_val
  expect_equal(nrow(mat), 1)
  expect_equal(ncol(mat), x$NumModules * nrow(x$ItemPool))
  expect_equal(ncol(mat), length(x$decisionvar_name))
})

test_that("pathway scope uses correct pathways", {
  x <- mst_design(itempool = mini_itempool,design = "1-3",pathway_length = 8)

  obj <- objective_term(
    x = x,
    attribute = "difficulty",
    applied_level = "Pathway-level",
    which_pathway = 2
  )

  expect_true(grepl("in Pathway 2", obj$name))

  # pathway 2 uses modules: stage1=1, stage2=3
  modules <- x$PathwayIndex[x$PathwayIndex$pathway_index==2, 2:3]
  mod_ids <- unlist(modules)
  expected_cols <- length(mod_ids) * nrow(x$ItemPool)
  expect_equal(ncol(obj$coef_val), x$NumModules * nrow(x$ItemPool))
  expect_equal(sum(obj$coef_val),sum(mini_itempool$difficulty)*2)
  expect_equal(obj$coef_val[c(1:nrow(mini_itempool),(1+2*nrow(mini_itempool)):(3*nrow(mini_itempool)))],rep(mini_itempool$difficulty,2))
})

test_that("panel scope builds full matrix", {
  x <- mst_design(itempool = mini_itempool,design = "1-3",pathway_length = 8)

  obj <- objective_term(
    x = x,
    attribute = "difficulty",
    applied_level = "Panel-level"
  )

  expect_true(grepl("in a panel", obj$name))
  expect_equal(nrow(obj$coef_val), 1)
  expect_equal(ncol(obj$coef_val), x$NumModules * nrow(x$ItemPool))
  expect_equal(sum(obj$coef_val),sum(mini_itempool$difficulty)*4)
})

test_that("objective_term handles categorical attributes", {
  x <- mst_design(itempool = mini_itempool,design = "1-3",pathway_length = 8)

  obj <- objective_term(
    x = x,
    attribute = "itemtype",
    cat_level = "TEI",
    applied_level = "Module-level",
    which_module = 1
  )
  whichTEI<-which(mini_itempool$itemtype=="TEI")
  expect_true(grepl("the number of items from 'TEI' category", obj$name))
  expect_equal(obj$sense, "max")
  expect_equal(obj$goal, NULL)
  expect_equal(which(as.vector(obj$coef_val)==1),whichTEI)
})
