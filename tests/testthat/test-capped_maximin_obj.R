test_that("maximin and capped_maximin differ in delta handling", {

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
    sense = "max"
  )

  obj2 <- objective_term(
    x = test_mstATA,
    attribute = "iif(theta=0)",
    applied_level = "Module-level",
    which_module = 1,
    sense = "max"
  )

  # -----------------------
  # MAXIMIN (delta fixed)
  # -----------------------
  maximin <- maximin_obj(
    x = test_mstATA,
    multiple_terms  = list(obj1, obj2),
    strategy_args = list(delta = 2)
  )

  expect_s3_class(maximin, "compiled_objective")
  expect_equal(maximin$sense, "max")

  # delta should NOT be a decision variable
  expect_false(any(grepl("delta", maximin$decisionvar_name_new)))

  # metadata should record delta as fixed
  expect_equal(maximin$notes$strategy, "maximin")
  expect_equal(maximin$notes$strategy_args$delta, c(2, 2))


  # -----------------------
  # CAPPED MAXIMIN (delta optimized)
  # -----------------------
  comp_capped <- capped_maximin_obj(
    x = test_mstATA,
    multiple_terms  = list(obj1, obj2)
  )

  expect_s3_class(comp_capped, "compiled_objective")
  expect_equal(comp_capped$sense, "max")

  # delta MUST appear as a decision variable
  expect_true(any(grepl("delta", comp_capped$decisionvar_name_new)))

  # metadata
  expect_equal(comp_capped$notes$strategy, "capped_maximin")
})


test_that("capped_maximin optimizes y-delta", {

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
    sense = "max"
  )

  obj2 <- objective_term(
    x = test_mstATA,
    attribute = "iif(theta=0)",
    applied_level = "Module-level",
    which_module = 1,
    sense = "max"
  )

  comp_capped <- capped_maximin_obj(
    x = test_mstATA,
    multiple_terms  = list(obj1, obj2)
  )

  expect_s3_class(comp_capped, "compiled_objective")
  expect_equal(comp_capped$sense, "max")

  # delta MUST appear as a decision variable
  expect_true(any(grepl("delta", comp_capped$decisionvar_name_new)))

  # metadata
  expect_equal(comp_capped$notes$strategy, "capped_maximin")

  expect_equal(apply(comp_capped$A_binary,1,sum),rep(c(sum(mini_itempool$`iif(theta=-1)`),sum(mini_itempool$`iif(theta=0)`)),each=2))
  expect_equal(as.vector(comp_capped$A_real),c(-1,-1,-1,-1,0,-1,0,-1))
  expect_equal(comp_capped$d,rep(0,4))
  expect_equal(comp_capped$operators,rep(c(">=","<="),2))
  expect_equal(comp_capped$C_real,c(1,-1))
})

test_that("capped_maximin deals with proportions", {

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
    sense = "max"
  )

  obj2 <- objective_term(
    x = test_mstATA,
    attribute = "iif(theta=0)",
    applied_level = "Module-level",
    which_module = 1,
    sense = "max"
  )


  comp_capped <- capped_maximin_obj(
    x = test_mstATA,
    multiple_terms  = list(obj1, obj2),
    strategy_args = list(proportions = c(1,2))
  )

  expect_s3_class(comp_capped, "compiled_objective")
  expect_equal(comp_capped$sense, "max")

  # delta MUST appear as a decision variable
  expect_true(any(grepl("delta", comp_capped$decisionvar_name_new)))

  # metadata
  expect_equal(comp_capped$notes$strategy, "capped_maximin")

  expect_equal(apply(comp_capped$A_binary,1,sum),rep(c(sum(mini_itempool$`iif(theta=-1)`),sum(mini_itempool$`iif(theta=0)`)),each=2))
  expect_equal(as.vector(comp_capped$A_real),c(-1,-1,-2,-2,0,-1,0,-1))
  expect_equal(comp_capped$d,rep(0,4))
  expect_equal(comp_capped$operators,rep(c(">=","<="),2))
  expect_equal(comp_capped$C_real,c(1,-1))
})
