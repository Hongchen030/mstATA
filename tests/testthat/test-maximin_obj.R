test_that("maximin compiles with multiple relative objective terms", {

  data("mini_itempool")

  test_mstATA <- mst_design(
    itempool = mini_itempool,
    design = "1-3-3",
    exclude_pathway = c("1-1-3", "1-3-1"),
    pathway_length = 8
  )

  obj1 <- objective_term(
    x = test_mstATA,
    attribute = "difficulty",
    applied_level = "Module-level",
    which_module = 1,
    sense = "max",
    goal = NULL
  )

  obj2 <- objective_term(
    x = test_mstATA,
    attribute = "discrimination",
    applied_level = "Module-level",
    which_module = 1,
    sense = "max",
    goal = NULL
  )

  comp <- maximin_obj(x = test_mstATA, list(obj1,obj2))

  expect_s3_class(comp, "compiled_objective")
  expect_equal(comp$sense, "max")

  # delta should NOT be a decision variable (default delta = Inf)
  expect_false(any(grepl("delta", comp$decisionvar_name_new)))

  # metadata
  expect_equal(comp$notes$strategy, "maximin")
})


test_that("maximin accepts user-specified delta", {

  data("mini_itempool")

  test_mstATA <- mst_design(
    itempool = mini_itempool,
    design = "1-3-3",
    pathway_length = 8
  )

  obj1 <- objective_term(
    x = test_mstATA,
    attribute = "difficulty",
    applied_level = "Panel-level",
    sense = "max"
  )

  obj2 <- objective_term(
    x = test_mstATA,
    attribute = "discrimination",
    applied_level = "Panel-level",
    sense = "max"
  )

  comp <- maximin_obj(x = test_mstATA, list(obj1,obj2),strategy_args = list(delta = 2))

  expect_equal(comp$notes$strategy_args$delta, c(2, 2))
  expect_false(any(grepl("delta", comp$decisionvar_name_new)))
})


test_that("maximin rejects absolute (goal-based) objectives", {

  data("mini_itempool")

  test_mstATA <- mst_design(
    itempool = mini_itempool,
    design = "1-3-3",
    pathway_length = 8
  )

  obj1 <- objective_term(
    x = test_mstATA,
    attribute = "difficulty",
    applied_level = "Panel-level",
    sense = "min",
    goal = 10
  )

  obj2 <- objective_term(
    x = test_mstATA,
    attribute = "discrimination",
    applied_level = "Panel-level",
    sense = "max"
  )

  expect_error(
    maximin_obj(
      x = test_mstATA,
      list(obj1, obj2)),
    "maximin requires all objective terms to have sense = 'max'."
  )
})


test_that("maximin requires at least two objective terms", {

  data("mini_itempool")

  test_mstATA <- mst_design(
    itempool = mini_itempool,
    design = "1-3-3",
    pathway_length = 8
  )

  obj <- objective_term(
    x = test_mstATA,
    attribute = "difficulty",
    applied_level = "Panel-level",
    sense = "max"
  )

  expect_error(
    maximin_obj(
      x = test_mstATA,
      obj
    ),
    "Multiple-term strategies require at least two terms. Use 'single' instead."
  )
})

test_that("maximin compiles with multiple relative objective terms and proportions", {

  data("mini_itempool")

  test_mstATA <- mst_design(
    itempool = mini_itempool,
    design = "1-3-3",
    exclude_pathway = c("1-1-3", "1-3-1"),
    pathway_length = 8
  )

  obj1 <- objective_term(
    x = test_mstATA,
    attribute = "iif(theta=-1)",
    applied_level = "Module-level",
    which_module = 1,
    sense = "max",
    goal = NULL
  )

  obj2 <- objective_term(
    x = test_mstATA,
    attribute = "iif(theta=0)",
    applied_level = "Module-level",
    which_module = 1,
    sense = "max",
    goal = NULL
  )

  comp <- maximin_obj(
    x = test_mstATA,
    list(obj1, obj2),
    strategy_args = list(proportions=c(1,2),delta=0.5)
  )


  expect_equal(apply(comp$A_binary,1,sum),rep(c(sum(mini_itempool$`iif(theta=-1)`),sum(mini_itempool$`iif(theta=0)`)),each=2))
  expect_equal(as.vector(comp$A_real),c(-1,-1,-2,-2))
  expect_equal(comp$operators,rep(c(">=","<="),2))
  expect_equal(comp$C_real,1L)
  expect_equal(comp$d,c(0,0.5,0,0.5))
})


test_that("delat = Inf", {

  data("mini_itempool")

  test_mstATA <- mst_design(
    itempool = mini_itempool,
    design = "1-3-3",
    exclude_pathway = c("1-1-3", "1-3-1"),
    pathway_length = 8
  )

  obj1 <- objective_term(
    x = test_mstATA,
    attribute = "iif(theta=-1)",
    applied_level = "Module-level",
    which_module = 1,
    sense = "max",
    goal = NULL
  )

  obj2 <- objective_term(
    x = test_mstATA,
    attribute = "iif(theta=0)",
    applied_level = "Module-level",
    which_module = 1,
    sense = "max",
    goal = NULL
  )


  comp <- maximin_obj(x = test_mstATA, list(obj1, obj2),
                            strategy_args = list(proportions=c(1,2)))

  expect_equal(apply(comp$A_binary,1,sum),c(sum(mini_itempool$`iif(theta=-1)`),sum(mini_itempool$`iif(theta=0)`)))
  expect_equal(as.vector(comp$A_real),c(-1,-2))
  expect_equal(comp$operators,c(">=",">="))
  expect_equal(comp$C_real,1L)
  expect_equal(comp$d,c(0,0))
})
