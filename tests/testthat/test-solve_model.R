library(Matrix)

solver_available <- function(solver) {
  switch(
    solver,
    gurobi = requireNamespace("gurobi", quietly = TRUE),
    HiGHS  = requireNamespace("highs", quietly = TRUE),
    GLPK   = requireNamespace("Rglpk", quietly = TRUE),
    lpsolve = requireNamespace("lpSolveAPI", quietly = TRUE),
    Symphony = requireNamespace("Rsymphony", quietly = TRUE),
    FALSE
  )
}

## ------------------------------------------------------------
## Helper: build a minimal mstATA_model
## ------------------------------------------------------------
make_model_spec <- function(A, d, operators, obj, lb, ub, vtype,sense) {
  structure(
    list(
      A = A,
      d = d,
      operators = operators,
      obj = obj,
      lb = lb,
      ub = ub,
      vtype = vtype,
      varname = paste0("x", seq_len(ncol(A))),
      name = paste0("c", seq_len(nrow(A))),
      sense = sense
    ),
    class = "mstATA_model"
  )
}

## ------------------------------------------------------------
## Helper: validate check dataframe
## ------------------------------------------------------------
expect_valid_check <- function(check, sense) {
  expect_true(is.data.frame(check))
  expect_true(all(c("LHS", "RHS", "residual") %in% names(check)))

  for (i in seq_len(nrow(check))) {

    if (sense[i] == "<=") {
      expect_lte(check$LHS[i], check$RHS[i] + 1e-8)
      expect_lte(check$residual[i], 1e-8)

    } else if (sense[i] == ">=") {
      expect_gte(check$LHS[i], check$RHS[i] - 1e-8)
      expect_lte(check$residual[i], 1e-8)

    } else if (sense[i] == "=") {
      expect_equal(check$LHS[i], check$RHS[i], tolerance = 1e-8)
      expect_lte(check$residual[i], 1e-8)
    }
  }

}

## ------------------------------------------------------------
## MILP test cases
## ------------------------------------------------------------
milp_cases <- list(

  ## OPTIMAL: binary knapsack
  knapsack1 = list(
    A = matrix(c(4, 2, 1, 3,
                 2, 1, 2, 1),
               nrow = 2, byrow = TRUE),
    d   = c(6, 4),
    operators = c("<=", "<="),
    obj   = c(10, 6, 4, 7),
    lb    = rep(0, 4),
    ub    = rep(1, 4),
    vtype = rep("B", 4),
    sense = "max",
    expected_status = "OPTIMAL",
    expected_obj = 17,
    expected_solution = c(0,1,1,1)
  ),

  knapsack2 = list(
    A = matrix(c(1, 1, 1,
                 2, 1, 1),
               nrow = 2, byrow = TRUE),
    d   = c(2, 4),
    operators = c("=", "<="),
    obj   = c(5,4,3),
    lb    = rep(0, 3),
    ub    = rep(1, 3),
    vtype = rep("B", 3),
    sense = "max",
    expected_status = "OPTIMAL",
    expected_obj = 9,
    expected_solution = c(1, 1,0)
  ),

  knapsack3 = list(
    A = matrix(c(3, 2), nrow = 1),
    d   = 10,
    operators = "<=",
    obj   = c(8,3),
    lb    = c(0, 0),
    ub    = c(1,Inf),
    vtype = c("B", "I"),
    sense = "max",
    expected_status = "OPTIMAL",
    expected_obj = 17,
    expected_solution = c(1, 3)
  ),

  knapsack4 = list(
    A = matrix(
      c(2, 1, 1,
        3, 2, 1),
      nrow = 2, byrow = TRUE
    ),
    d   = c(3,5),
    operators = c(">=","<="),
    obj   = c(6,5,4),
    lb    = c(0, 0,0),
    ub    = c(1,1,1),
    vtype = c("B", "B","B"),
    sense = "max",
    expected_status = "OPTIMAL",
    expected_obj = 11,
    expected_solution = c(1,1,0)
  ),

  knapsack5 = list(
    A = matrix(
      c(1, 1,
        1, 1),
      nrow = 2, byrow = TRUE
    ),
    d = c(1, 3),
    operators = c("<=", ">="),
    obj = c(1, 1),
    lb = c(0, 0),
    ub = c(1, 1),
    vtype = c("B", "B"),
    sense = "max",
    expected_status = "INFEASIBLE",
    expected_obj = NA_real_,
    expected_solution = NULL
  ),

  knapsack6 = list(
    A = matrix(
      c(1, 0),
      nrow = 1, byrow = TRUE
    ),
    d   = 5,
    operators = "<=",
    obj   = c(1,2),
    lb    = c(0,0),
    ub    = c(5,Inf),
    vtype = c("C","C"),
    sense = "max",
    expected_status = "UNBOUNDED",
    expected_obj = NA_real_,
    expected_solution = NULL
  ),

  knapsack7 = list(
    A = matrix(
      c(
        2, 3, 1, 4, 2,
        1, 1, 2, 1, 3,
        3, 2, 2, 1, 1
      ),
      nrow = 3, byrow = TRUE
    ),
    d   = c(7,5,6),
    operators = c("<=","<=","<="),
    obj   = c(6,5,4,7,3),
    lb    = c(0,0,0,0,0),
    ub    = c(1,1,1,1,1),
    vtype = c("B","B","B","B","B"),
    sense = "max",
    expected_status = "OPTIMAL",
    expected_obj = 17,
    expected_solution = c(1,0,1,1,0)
  )
)

## ------------------------------------------------------------
## Solvers to test
## ------------------------------------------------------------
solvers <- c("gurobi","lpsolve","HiGHS","Symphony","GLPK")

## ------------------------------------------------------------
## Parametrized test
## ------------------------------------------------------------
test_that("solve_model returns correct results across solvers", {

  for (case_name in names(milp_cases)) {

    case <- milp_cases[[case_name]]
    model_spec <- make_model_spec(
      A = case$A,
      d = case$d,
      operators = case$operators,
      obj = case$obj,
      lb = case$lb,
      ub = case$ub,
      vtype = case$vtype,
      sense = case$sense
    )

    for (solver in solvers) {

      ## skip unavailable solvers gracefully
      skip_if_not(
        solver_available(solver),
        paste("Solver not available:", solver)
      )


      res <- solve_model(
        model_spec,
        solver = solver,
        time_limit = 99999,
        check_feasibility = TRUE
      )

      ## ---- status ----
      expect_equal(
        res$solution$solution_found,
        case$expected_status,
        info = paste("Case:", case_name, "Solver:", solver)
      )

      if (res$solution$solution_found %in% c("OPTIMAL", "FEASIBLE")) {

        expect_false(is.null(res$solution$best_solution),
                     info = paste("Case:", case_name, "Solver:", solver))

        expect_true(is.finite(res$solution$objval),
                    info = paste("Case:", case_name, "Solver:", solver))

        if (!is.null(case$expected_obj)) {
          expect_equal(res$solution$objval, case$expected_obj, tolerance = 1e-6)
        }

        if (!is.null(case$expected_solution)) {
          expect_equal(
            as.numeric(res$solution$best_solution),
            as.numeric(case$expected_solution),
            tolerance = 1e-6
          )
        }

        expect_valid_check(res$solution$check, case$operators)


      } else {

        expect_true(is.null(res$solution$best_solution),
                    info = paste("Case:", case_name, "Solver:", solver))
        expect_true(is.na(res$solution$objval),
                    info = paste("Case:", case_name, "Solver:", solver))
        expect_true(is.null(res$solution$check),
                    info = paste("Case:", case_name, "Solver:", solver))
      }
    }


  }
})

