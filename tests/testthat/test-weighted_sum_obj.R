test_that("weighted_sum_obj validates input classes", {
  x <- make_test_mstATA_BU()              # helper you already use elsewhere
  obj_set <- list()                # not an objective_set

  expect_error(
    weighted_sum_obj(x, obj_set),
    "'obj_set' must be a non-empty list."
  )
})

test_that("weighted_sum_obj works for two objectives without goal", {
  x <- mst_design(itempool = mini_itempool,design = "1-3",module_length = c(3,4,4,4))

  term1 <- objective_term(x = x,
                         attribute = "iif(theta=-1)",cat_level = NULL,sense = "max",
                         applied_level = "Module-level",which_module = 1,goal = NULL)
  term2 <- objective_term(x = x,
                          attribute = "iif(theta=1)",cat_level = NULL,sense = "max",
                          applied_level = "Module-level",which_module = 1,goal = NULL)

  obj <- weighted_sum_obj(x, list(term1,term2),strategy_args = list(weights = c(1,1)))

  expect_s3_class(obj, "compiled_objective")
  expect_equal(nrow(obj$A_binary), 2L)
  expect_equal(ncol(obj$A_binary), length(x$decisionvar_name))
  expect_equal(length(obj$operators), 2L)
  expect_equal(obj$operators,c(">=", ">="))
  expect_equal(obj$d,rep(0,2))
  expect_equal(apply(obj$A_binary,1,sum),
               c(sum(mini_itempool[["iif(theta=-1)"]]),sum(mini_itempool[["iif(theta=1)"]])))
  expect_equal(as.vector(obj$A_real),c(-1,0,0,-1))
})


test_that("compile_weighted_sum_ creates two constraints when goal is present", {
  x <- mst_design(itempool = mini_itempool,design = "1-3",module_length = c(3,4,4,4))

  term1 <- objective_term(x = x,
                          attribute = "iif(theta=-1)",cat_level = NULL,
                          applied_level = "Module-level",which_module = 1,goal = 10)
  term2 <- objective_term(x = x,
                          attribute = "iif(theta=1)",cat_level = NULL,
                          applied_level = "Module-level",which_module = 1,goal = 10)

  obj <- weighted_sum_obj(x, list(term1,term2),strategy_args = list(weights = c(1,1)))

  expect_equal(nrow(obj$A_binary), 4L)
  expect_equal(length(obj$operators), 4L)
  expect_setequal(obj$operators, rep(c(">=", "<="),2))
  expect_equal(apply(obj$A_binary,1,sum),
               rep(c(sum(mini_itempool[["iif(theta=-1)"]]),sum(mini_itempool[["iif(theta=1)"]])),each=2))
  expect_equal(as.vector(obj$A_real),c(1,-1,0,0,0,0,1,-1))
  expect_equal(obj$d,rep(10,4))
  expect_equal(obj$C_real,c(1,1))
})

test_that("compile_weighted_sum_ handles multiple objectives with mixed goals", {
  x <- mst_design(itempool = mini_itempool,design = "1-3",module_length = c(3,4,4,4))

  term1 <- objective_term(x = x,
                          attribute = "iif(theta=-1)",cat_level = NULL,sense = "max",
                          applied_level = "Module-level",which_module = 1,goal = NULL)
  term2 <- objective_term(x = x,
                          attribute = "iif(theta=1)",cat_level = NULL,
                          applied_level = "Module-level",which_module = 1,goal = 10)

  obj <- weighted_sum_obj(x, list(term1,term2),strategy_args = list(weights = c(1,2)))


  # rows: 1 (term1) + 2 (term2)
  expect_equal(nrow(obj$A_binary), 3L)
  expect_equal(length(obj$decisionvar_name_new), 2L)
  expect_equal(length(obj$C_real), 2L)
  expect_equal(apply(obj$A_binary,1,sum),c(sum(mini_itempool$`iif(theta=-1)`),rep(sum(mini_itempool$`iif(theta=1)`),2)))
  expect_equal(obj$d,c(0,10,10))
  expect_equal(obj$operators,c(">=",">=","<="))
  expect_equal(as.vector(obj$A_real),c(-1,0,0,0,1,-1))
  expect_equal(obj$C_real,c(-1,2))
})


