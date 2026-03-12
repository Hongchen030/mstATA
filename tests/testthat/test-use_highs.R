test_that("use_highs returns NULL when highs is not installed", {

  skip_if(
    requireNamespace("highs", quietly = TRUE),
    "highs is installed"
  )

  A <- Matrix::Matrix(0, nrow = 1, ncol = 1, sparse = TRUE)
  rhs <- 0
  sense <- "="
  obj <- 0
  lb <- 0
  ub <- 1
  vtype <- "B"
  varname <- "x1"
  modelsense <- "max"
  params <- list(threads = 4,time_limit = 99999,mip_rel_gap = 1e-2,mip_abs_gap = 0.0,
                 presolve = "on",parallel = "on",log_to_console = FALSE,
                 random_seed = 200)
  rowname <- "r1"

  res <- use_highs(
    A, rhs, sense, obj, lb, ub,
    vtype, varname, modelsense,
    params, rowname
  )

  expect_null(res)
})


test_that("Binary knapsack solves correctly", {

  A <- Matrix::Matrix(c(4, 2, 1), nrow = 1,sparse = TRUE)
  rhs <- 6
  sense <- "<="
  obj <- c(10, 6, 4)
  vtype <- c("B", "B", "B")

  res <- use_highs(
    A = A,
    obj = obj,
    rhs = rhs,
    sense = sense,
    vtype = vtype,
    lb= rep(0,3),ub = rep(1,3),
    varname = c("x[1]","x[2]","x[3]"),
    modelsense = "max",
    params = list(threads = 4,time_limit = 99999,mip_rel_gap = 1e-2,mip_abs_gap = 0.0,
                  presolve = "on",parallel = "on",log_to_console = FALSE,
                  random_seed = 200),
    rowname = "con1"
  )

  expect_equal(res$solution_found, "OPTIMAL")
  expect_equal(res$objval, 16)
  expect_equal(as.numeric(res$best_solution), c(1, 1, 0))
})

test_that("Mixed binary-continuous MILP solves correctly", {

  A <- rbind(
    c(1, 2, 0),   # x + 2y <= 6
    c(1, 0, -4)   # x - 4z <= 0
  )

  rhs <- c(6, 0)
  sense <- c("<=", "<=")
  obj <- c(3, 5, 0)
  vtype <- c("C", "C", "B")

  res <- use_highs(
    A = A,
    obj = obj,
    rhs = rhs,
    sense = sense,
    vtype = vtype,
    lb= c(0,0,0),ub = c(Inf,Inf,1),
    varname = c("x[1]","x[2]","x[3]"),
    modelsense = "max",
    params = list(threads = 4,time_limit = 99999,mip_rel_gap = 1e-2,mip_abs_gap = 0.0,
                  presolve = "on",parallel = "on",log_to_console = FALSE,
                  random_seed = 200),
    rowname = c("con1","con2")
  )


  expect_equal(res$solution_found, "OPTIMAL")
  expect_equal(res$objval, 17)

  sol <- as.numeric(res$best_solution)
  expect_equal(sol[1], 4, tolerance = 1e-6)
  expect_equal(sol[2], 1, tolerance = 1e-6)
  expect_equal(sol[3], 1)
})


test_that("Equality constraint with binaries is respected", {

  A <- matrix(c(1, 1), nrow = 1)
  rhs <- 1
  sense <- "="
  obj <- c(1, 1)
  vtype <- c("B", "B")

  res <- use_highs(
    A = A,
    obj = obj,
    rhs = rhs,
    sense = sense,
    vtype = vtype,
    lb= c(0,0),ub = c(1,1),
    varname = c("x[1]","x[2]"),
    modelsense = "max",
    params = list(threads = 4,time_limit = 99999,mip_rel_gap = 1e-2,mip_abs_gap = 0.0,
                  presolve = "on",parallel = "on",log_to_console = FALSE,
                  random_seed = 200),
    rowname = "con1"
  )

  expect_equal(res$solution_found, "OPTIMAL")
  expect_equal(res$objval, 1)

  sol <- as.numeric(res$best_solution)
  expect_equal(sum(sol), 1)
  expect_true(all(sol %in% c(0, 1)))
})

test_that("Enemy-item constraint is enforced", {

  A <- rbind(
    c(1, 1, 1),  # select exactly 2 items
    c(1, 1, 0)   # item 1 and 2 are enemies
  )

  rhs <- c(2, 1)
  sense <- c("=", "<=")
  obj <- c(5, 4, 3)
  vtype <- c("B", "B", "B")

  res <- use_highs(
    A = A,
    obj = obj,
    rhs = rhs,
    sense = sense,
    vtype = vtype,
    lb= c(0,0,0),ub = c(1,1,1),
    varname = c("x[1]","x[2]","x[3]"),
    modelsense = "max",
    params = list(threads = 4,time_limit = 99999,mip_rel_gap = 1e-2,mip_abs_gap = 0.0,
                  presolve = "on",parallel = "on",log_to_console = FALSE,
                  random_seed = 200),
    rowname = c("con1","con2")
  )

  expect_equal(res$solution_found, "OPTIMAL")
  expect_equal(res$objval, 8)
  expect_equal(as.numeric(res$best_solution), c(1, 0, 1))
})


test_that("Infeasible MILP is correctly detected", {

  A <- rbind(
    c(1),
    c(1)
  )

  rhs <- c(2, 1)
  sense <- c(">=", "<=")
  obj <- 1
  vtype <- "I"

  res <- use_highs(
    A = A,
    obj = obj,
    rhs = rhs,
    sense = sense,
    vtype = vtype,
    modelsense = "min",
    lb=0,ub=Inf,
    params = list(threads = 4,time_limit = 99999,mip_rel_gap = 1e-2,mip_abs_gap = 0.0,
                  presolve = "on",parallel = "on",log_to_console = FALSE,
                  random_seed = 200),
    varname = "x[1]",
    rowname = c("con1","con2")
  )
  expect_equal(res$solution_found, "INFEASIBLE")
  expect_true(is.null(res$best_solution) || all(is.na(res$best_solution)))
})
