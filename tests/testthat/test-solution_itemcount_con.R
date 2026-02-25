
test_that("solution_itemcount_con basic functionality", {

  # ---- Fake mstATA_design object ----
  fake_mstATA <- function(PoolSize = 4, NumModules = 2) {
    decnames <- paste0(
      rep(paste0("item ", 1:PoolSize), NumModules),
      rep(paste0(" in module ", 1:NumModules), each = PoolSize)
    )
    x <- list(
      ItemPool = data.frame(id = 1:PoolSize),
      NumModules = NumModules,
      decisionvar_name = decnames
    )
    class(x) <- "mstATA_design"
    x
  }

  x <- fake_mstATA(PoolSize = 4, NumModules = 2)
  num_panels<-3
  # Total DV columns = num_panels * (PoolSize*NumModules) + PoolSize
  expect_equal(length(x$decisionvar_name), 8)

  # ---- MIN only ----
  out_min <- solution_itemcount_con(x,operator = ">=",target_num = 3)

  expect_s3_class(out_min, "mstATA_constraint")
  expect_equal(nrow(out_min$A_binary), 1)
  expect_equal(ncol(out_min$A_binary), 4)
  expect_equal(out_min$operators, ">=")
  expect_equal(out_min$d, 3)

  # Check that only s's have coefficient 1
  s_index <- 1:4
  expect_equal(as.vector(out_min$A_binary[1, s_index]), rep(1, 4))

  # ---- MAX only ----
  out_max <- solution_itemcount_con(x,operator = "<=",target_num  = 2)
  expect_equal(out_max$operators, "<=")
  expect_equal(out_max$d, 2)

  # ---- BOTH min and max ----
  out_both <- combine_constraints(list(solution_itemcount_con(x,operator = ">=",1),
                                       solution_itemcount_con(x,operator = "<=",4)))
  expect_equal(nrow(out_both$A_binary), 2)
  expect_equal(out_both$operators, c(">=","<="))
  expect_equal(out_both$d, c(1,4))

})
