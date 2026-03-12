test_that("goal_programming requires multiple absolute objective terms", {

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


  obj<-goal_programming_obj(test_mstATA, list(obj1, obj2))

  expect_equal(nrow(obj$A_binary),4)
  expect_equal(nrow(obj$A_real),4)
  expect_equal(ncol(obj$A_real),2)
  expect_equal(apply(obj$A_binary,1,sum),rep(c(sum(mini_itempool$`iif(theta=-1)`),sum(mini_itempool$`iif(theta=1)`)),each=2))
  expect_equal(as.vector(obj$A_real),c(1,-1,0,0,0,0,1,-1))
  expect_equal(obj$d,rep(6,4))
  expect_equal(obj$operators,c(">=","<=",">=","<="))
})

test_that("goal_programming rejects relative objectives", {

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
    sense = "max", goal = NULL
  )

  obj2 <- objective_term(
    test_mstATA, "difficulty", NULL,
    "Module-level", which_module = 2,
    sense = "max", goal = NULL
  )

  expect_error(goal_programming_obj(x = test_mstATA, list(obj1, obj2)),
               "goal_programming requires all objective terms")
})

test_that("goal_programming creates correct deviation variables", {

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

  ## one_dev: one deviation per term
  compiled_one <- goal_programming_obj(test_mstATA,list(obj1, obj2),strategy_args = list(mode = "one_dev"))

  expect_s3_class(compiled_one, "compiled_objective")
  expect_equal(length(compiled_one$decisionvar_name_new), 2)
  expect_true(all(compiled_one$decisionvar_type_new == "C"))
  expect_equal(compiled_one$sense, "min")
  expect_equal(as.vector(compiled_one$A_real),c(1,-1,0,0,0,0,1,-1))

  ## two_dev: two deviations per term

  compiled_two <- goal_programming_obj(test_mstATA,list(obj1, obj2), strategy_args = list(mode = "two_dev"))

  expect_equal(length(compiled_two$decisionvar_name_new), 4)
  expect_true(all(compiled_two$decisionvar_type_new == "C"))
  expect_equal(as.vector(compiled_two$A_real),c(1,0,0,0,
                                                0,-1,0,0,
                                                0,0,1,0,
                                                0,0,0,-1))
})

test_that("goal_programming validates weights correctly", {

  data("mini_itempool")

  test_mstATA <- mst_design(
    itempool = mini_itempool,
    design = "1-3-3",
    pathway_length = 8
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


  compiled_two<-goal_programming_obj(test_mstATA, list(obj1, obj2),strategy_args = list(weights = c(1, 0.5)))
  expect_silent(
    goal_programming_obj(test_mstATA, list(obj1, obj2),strategy_args = list(weights = c(1, 0.5)))
  )
  expect_equal(compiled_two$C_real,c(1,0.5))
})
