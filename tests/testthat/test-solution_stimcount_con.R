test_that("solution_stimcount_con basic functionality works", {

  x<-make_test_mstATA_BU()

  total_cols <- nrow(x$ItemPool)

  # ---- MIN only ----
  con_min <- solution_stimcount_con(x,">=",1)

  expect_s3_class(con_min, "mstATA_constraint")
  expect_equal(nrow(con_min$A_binary), 1)
  expect_equal(ncol(con_min$A_binary), total_cols)
  expect_equal(con_min$operators, ">=")
  expect_equal(con_min$d, 1)

  # Check that only pivot s columns are 1
  s_cols <- seq_len(total_cols)
  which_pivots<-which(test_itempool$pivot_item=="Y")

  expect_equal(as.vector(con_min$A_binary[1, which_pivots]), rep(1,length(which_pivots)))
  expect_equal(sum(con_min$A_binary[1, which_pivots]), length(which_pivots))



  # ---- MAX only ----
  con_max <- solution_stimcount_con(x,"<=", 2)
  expect_equal(con_max$operators, "<=")
  expect_equal(con_max$d, 2)

  # ---- MIN and MAX ----
  con_both <- combine_constraints(list(solution_stimcount_con(x,">=",1),
                                       solution_stimcount_con(x,"<=",2)))
  expect_equal(nrow(con_both$A_binary), 2)
  expect_equal(con_both$operators, c(">=","<="))
  expect_equal(con_both$d, c(1,2))

})
