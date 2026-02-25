test_that("minimax requires multiple absolute objective terms", {

  data("mini_itempool")

  test_mstATA <- mst_design(
    itempool        = mini_itempool,
    design          = "1-3-3",
    exclude_pathway = c("1-1-3", "1-3-1"),
    pathway_length  = 8
  )

  # Absolute objectives (goal specified)
  obj1 <- objective_term(
    test_mstATA, "iif(theta=-1)", NULL,
    "Module-level", which_module = 2,
    sense = "min", goal = 6
  )

  obj2 <- objective_term(
    test_mstATA, "iif(theta=1)", NULL,
    "Module-level", which_module = 4,
    sense = "min", goal = 6
  )


  obj<-minimax_obj(test_mstATA, multiple_terms = list(obj1, obj2))

  expect_equal(nrow(obj$A_binary),4)
  expect_equal(nrow(obj$A_real),4)
  expect_equal(ncol(obj$A_real),1)
  expect_equal(apply(obj$A_binary,1,sum),rep(c(sum(mini_itempool$`iif(theta=-1)`),sum(mini_itempool$`iif(theta=1)`)),each=2))
  expect_equal(as.vector(obj$A_real),rep(c(1,-1),2))
  expect_equal(obj$d,rep(6,4))
  expect_equal(obj$operators,c(">=","<=",">=","<="))
})

test_that("minimax rejects relative objectives", {

  data("mini_itempool")

  test_mstATA <- mst_design(
    itempool        = mini_itempool,
    design          = "1-3-3",
    pathway_length  = 8
  )

  # Relative objective (goal = NULL)
  obj1 <- objective_term(
    test_mstATA, "difficulty", NULL,
    "Module-level", which_module = 1,
    sense = "min", goal = NULL
  )

  obj2 <- objective_term(
    test_mstATA, "difficulty", NULL,
    "Module-level", which_module = 2,
    sense = "min", goal = NULL
  )

  expect_error(minimax_obj(x = test_mstATA,list(obj1, obj2)),
               "minimax: all terms must be absolute objectives")
})

test_that("minimax creates correct deviation variables", {

  data("mini_itempool")

  test_mstATA <- mst_design(
    itempool        = mini_itempool,
    design          = "1-3-3",
    pathway_length  = 8
  )

  obj1 <- objective_term(
    test_mstATA, "difficulty", NULL,
    "Module-level", which_module = 1,
    sense = "min", goal = 5
  )

  obj2 <- objective_term(
    test_mstATA, "difficulty", NULL,
    "Module-level", which_module = 2,
    sense = "min", goal = 6
  )


  ## one_dev: one deviations per term
  compiled_one <- minimax_obj(test_mstATA, list(obj1, obj2),strategy_args = list(mode = "one_dev"))

  expect_s3_class(compiled_one, "compiled_objective")
  expect_equal(length(compiled_one$decisionvar_name_new), 1)
  expect_true(all(compiled_one$decisionvar_type_new == "C"))
  expect_equal(compiled_one$sense, "min")
  expect_equal(as.vector(compiled_one$A_real),rep(c(1,-1),2))

  ## two_dev: two deviations per term
  compiled_two <- minimax_obj(test_mstATA, list(obj1, obj2),strategy_args = list(mode = "two_dev"))

  expect_equal(length(compiled_two$decisionvar_name_new), 2)
  expect_true(all(compiled_two$decisionvar_type_new == "C"))
  expect_equal(as.vector(compiled_two$A_real),c(1,0,1,0,0,-1,0,-1))
})

test_that("minimax one_dev produces a single deviation variable", {

  data("mini_itempool")

  x <- mst_design(
    itempool = mini_itempool,
    design = "1-3-3",
    pathway_length = 8
  )

  obj1 <- objective_term(
    x = x,
    attribute = "difficulty",
    applied_level = "Module-level",
    which_module = 1,
    sense = "min",
    goal = 0
  )

  obj2 <- objective_term(
    x = x,
    attribute = "difficulty",
    applied_level = "Module-level",
    which_module = 2,
    sense = "min",
    goal = 0
  )


  compiled<- minimax_obj(x,list(obj1, obj2),strategy_args = list(mode = "one_dev"))

  expect_s3_class(compiled, "compiled_objective")
  expect_equal(compiled$sense, "min")

  # exactly one auxiliary deviation variable
  expect_equal(length(compiled$decisionvar_name_new), 1)
  expect_true(all(compiled$decisionvar_type_new == "C"))

  # constraints must exist
  expect_true(nrow(compiled$A_real) > 0)
})


test_that("minimax two_dev produces two deviation variables", {

  data("mini_itempool")

  x <- mst_design(
    itempool = mini_itempool,
    design = "1-3-3",
    pathway_length = 8
  )

  obj1 <- objective_term(
    x = x,
    attribute = "difficulty",
    applied_level = "Module-level",
    which_module = 1,
    sense = "min",
    goal = 0
  )

  obj2 <- objective_term(
    x = x,
    attribute = "difficulty",
    applied_level = "Module-level",
    which_module = 2,
    sense = "min",
    goal = 0
  )


  compiled<- minimax_obj(x,  list(obj1, obj2), strategy_args = list(mode = "two_dev"))

  expect_s3_class(compiled, "compiled_objective")
  expect_equal(compiled$sense, "min")

  # two auxiliary deviation variables: d+ and d-
  expect_equal(length(compiled$decisionvar_name_new), 2)
  expect_true(all(compiled$decisionvar_type_new == "C"))
})



