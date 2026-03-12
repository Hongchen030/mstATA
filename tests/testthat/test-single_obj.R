test_that("single_obj rejects invalid inputs", {
  data("mini_itempool")

  x <- mst_design(itempool = mini_itempool,
                  design = "1-3",
                  pathway_length = 5)
  obj<-objective_term(x = x,attribute = "itemtype",cat_level = "MC",
                      applied_level = "Module-level",which_module = 1,
                      which_pathway = NULL,sense = "max",goal = NULL)

  expect_error(
    single_obj(x = 1, single_term = obj),
    "Input 'x' must be an object of class 'mstATA_design'."
  )

  # Invalid: single_term must be objective_term()
  expect_error(
    single_obj(x = x, single_term = list(a=1)),
    "'obj_set' must all be 'mstATA_objective' objects."
  )
})

test_that("single_obj requires exactly one objective term", {
  data("mini_itempool")

  x <- mst_design(itempool = mini_itempool,
                  design = "1-3",
                  pathway_length = 5)

  term1 <- objective_term(
    x = x,
    attribute = "difficulty",
    cat_level = NULL,
    applied_level = "Panel-level",
    sense = "max"
  )

  # Correct
  expect_silent(single_obj(x, term1))

  # Incorrect: more than one term
  term2 <- objective_term(
    x = x,
    attribute = "discrimination",
    cat_level = NULL,
    applied_level = "Panel-level",
    sense = "max"
  )

  expect_error(
    single_obj(x, single_term = list(term1, term2)),
    "strategy='single' requires exactly one term."
  )
})

test_that("single_obj returns a valid compiled_objective", {
  data("mini_itempool")

  PoolSize<-nrow(mini_itempool)
  x <- mst_design(itempool = mini_itempool,
                  design = "1-3",
                  pathway_length = 5)

  term <- objective_term(
    x = x,
    attribute = "iif(theta=0)",
    cat_level = NULL,
    applied_level = "Module-level",
    which_module = 1,
    sense = "max"
  )

  obj <- single_obj(x, term)

  # Class check
  expect_s3_class(obj, "compiled_objective")

  # Required fields exist
  expected_fields <- c(
    "name","specification", "A_binary", "A_real",
    "operators", "d",
    "C_binary", "C_real",
    "decisionvar_name_new", "decisionvar_type_new",
    "sense",
    "notes"
  )

  expect_true(all(expected_fields %in% names(obj)))

  # Sense must match
  expect_equal(obj$sense, "max")

  # A_binary should be non-null and correct number of columns
  expect_true(inherits(obj$A_binary, "Matrix"))
  expect_equal(ncol(obj$A_binary), x$NumModules * nrow(x$ItemPool))

  # operator and d must be the correct length
  expect_equal(length(obj$operators), nrow(obj$A_binary))
  expect_equal(length(obj$d), nrow(obj$A_binary))

  # Penalty vectors correct length
  expect_length(obj$C_binary, ncol(obj$A_binary))
  expect_length(obj$C_real, ncol(obj$A_real))

  # New real decision vars should match A_real's dimension
  expect_equal(length(obj$decisionvar_name_new), ncol(obj$A_real))

  expect_equal(as.vector(obj$A_binary[1,1:PoolSize]),mini_itempool[["iif(theta=0)"]])
  expect_equal(as.vector(obj$A_real[1,1]),-1)
  expect_equal(obj$d,0)
  expect_equal(obj$operators,">=")
})

