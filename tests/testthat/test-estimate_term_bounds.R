test_that("estimate_term_bounds works for module-length case", {

  # Mock mstATA object
  x <- list(
    NumStages   = 1,
    NumModules  = 2,
    NumPathways = 2,
    NumPanels   = 1,
    ItemPool    = data.frame(a = 1:6),  # PoolSize = 6
    ModuleIndex = data.frame(ModuleLength = c(2, 3)),
    PathwayIndex = NULL,
    decisionvar_name = c("x[1,1]","x[1,2]","x[1,3]","x[1,4]","x[1,5]","x[1,6]",
                         "x[2,1]","x[2,2]","x[2,3]","x[2,4]","x[2,5]","x[2,6]")
  )
  class(x) <- "mstATA_design"

  PoolSize <- nrow(x$ItemPool)

  # a_vec for 2 modules each with PoolSize entries
  # Module 1: coefficients 1–6
  # Module 2: coefficients 2–7
  a_vec <- c(1:6, 2:7)

  # Expected module 1 bounds:
  # LB1 = sum of two smallest from 1:6 = 1 + 2 = 3
  # UB1 = sum of two largest  from 1:6 = 6 + 5 = 11
  #
  # Module 2 bounds:
  # LB2 = sum of three smallest from 2:7 = 2 + 3 + 4 = 9
  # UB2 = sum of three largest  from 2:7 = 7 + 6 + 5 = 18
  #
  # Total:
  # lower_bound = LB1 + LB2 = 3 + 9 = 12
  # upper_bound = UB1 + UB2 = 11 + 18 = 29

  out <- estimate_term_bounds(x, a_vec)

  expect_named(out, c("lower_bound", "upper_bound"))
  expect_equal(out[["lower_bound"]], 12)
  expect_equal(out[["upper_bound"]], 29)
})


test_that("estimate_term_bounds works for pathway-length case", {

  # Mock mstATA object (NO ModuleLength)
  x <- list(
    NumStages   = 2,
    NumModules  = 3,
    NumPathways = 2,
    NumPanels   = 1,
    ItemPool    = data.frame(a = 1:5),  # PoolSize = 5
    ModuleIndex = data.frame(ModuleLength = NULL), # triggers pathway case
    PathwayIndex = data.frame(
      stage1 = c(1, 1),
      stage2 = c(2, 3),
      pathway_index = c(1, 2),
      PathwayLength = c(3, 3)
    ),
    decisionvar_name = c("x[1,1]","x[1,2]","x[1,3]","x[1,4]","x[1,5]",
                         "x[2,1]","x[2,2]","x[2,3]","x[2,4]","x[2,5]",
                         "x[3,1]","x[3,2]","x[3,3]","x[3,4]","x[3,5]")
  )
  class(x) <- "mstATA_design"

  PoolSize <- nrow(x$ItemPool)

  # a_vec structure = concatenation of module coefficients:
  # Module 1: 1–5
  # Module 2: 2–6
  # Module 3: 3–7
  a_vec <- c(1:5, 2:6, 3:7)

  # Pathway 1 uses modules (1, 2)
  # Extracted sub_a = c(1:5 from mod1, 2:6 from mod2)
  # LB1 = 1 + 2 + 2 = 5
  # UB1 = 6 + 5 + 5= 16
  #
  # Pathway 2 uses modules (1, 3)
  # sub_a = c(1:5 from mod1, 3:7 from mod3)
  # LB2 = 1 + 2 + 3 = 6
  # UB2 = 7 + 6 + 5 = 18

  # According to your current function:
  # lower_bound = min(LB1, LB2) = min(5, 6) = 5
  # upper_bound = sum(UB) = UB1 + UB2 = 16 + 18 = 34

  out <- estimate_term_bounds(x, a_vec)

  expect_named(out, c("lower_bound", "upper_bound"))
  expect_equal(out[["lower_bound"]], 5)
  expect_equal(out[["upper_bound"]], 34)
})


test_that("estimate_term_bounds handles negative coefficients correctly", {

  x <- list(
    NumStages   = 1,
    NumModules  = 1,
    NumPathways = 1,
    NumPanels   = 1,
    ItemPool    = data.frame(a = 1:4), # PoolSize = 4
    ModuleIndex = data.frame(ModuleLength = c(2)),
    PathwayIndex = NULL,
    decisionvar_name = c("x[1,1]","x[1,2]","x[1,3]","x[1,4]")
  )
  class(x) <- "mstATA_design"

  # a_vec for one module: negative + positive
  a_vec <- c(-5, -2, 3, 10)

  # For module length 2:
  # LB = sum of the smallest 2 values = -5 + (-2) = -7
  # UB = sum of the largest  2 values = 3 + 10 = 13

  out <- estimate_term_bounds(x, a_vec)

  expect_equal(out[["lower_bound"]], -7)
  expect_equal(out[["upper_bound"]], 13)
})

test_that("estimate_term_bounds respects item-module eligibility", {

  ## ---- mock mstATA_design ----
  x <- list(
    NumStages   = 2L,
    NumModules  = 2L,
    NumPathways = 1L,
    NumPanels   = 1L,
    ItemPool    = data.frame(item_id = 1:4),
    decisionvar_name = c(
      "x[1,1]", "x[1,2]",   # module 1 has items 1,2
      "x[2,2]", "x[2,3]"    # module 2 has items 2,3
    ),
    ModuleIndex = list(
      ModuleLength = c(1L, 1L)
    ),
    PathwayIndex = NULL
  )
  class(x) <- "mstATA_design"

  ## coefficients correspond to decisionvar_name order
  a_vec <- c(1, 5, 2, 10)

  ## ---- run ----
  bounds <- estimate_term_bounds(x, a_vec)

  ## ---- expected ----
  ## module 1: min = 1, max = 5
  ## module 2: min = 2, max = 10
  ## total LB = 1 + 2 = 3
  ## total UB = 5 + 10 = 15

  expect_equal(bounds[["lower_bound"]], 3)
  expect_equal(bounds[["upper_bound"]], 15)
})

