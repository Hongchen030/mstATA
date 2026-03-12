test_that("non-solution-level constraint returns FALSE", {

  data("reading_itempool")
  pivot_stim_map <- create_pivot_stimulus_map(itempool   = reading_itempool,
                                              stimulus   = "stimulus",pivot_item = "pivot_item")
  test_mstATA <- mst_design(itempool = reading_itempool,design = "1-3-3",
                            module_length = c(14,12,12,12,6,6,6),
                            pivot_stim_map = pivot_stim_map)
  discrete_con<-test_itemcat_con(x = test_mstATA,attribute = "itemtype",cat_levels = "TEI",target_num = 2,
                                 operator = "=",which_module = 1)
  stimulus_con<-test_stimcat_con(x = test_mstATA,
                                 attribute = "stimulus_type",cat_levels = "history",target_num = 1,,
                                 operator = "=",which_module = 1)
  itemset_con<-stim_itemcount_con(x = test_mstATA,
                                  min = 1,max = 5,which_module = 1)
  solution_con<-combine_constraints(list(solution_itemcount_con(x = test_mstATA,">=",40),
                                         solution_itemcount_con(x = test_mstATA,"<=",50)))
  expect_false(is_multipanel_solution_constraint(discrete_con, test_mstATA))
  expect_false(is_multipanel_solution_constraint(stimulus_con,test_mstATA))
  expect_false(is_multipanel_solution_constraint(itemset_con,test_mstATA))
  expect_true(is_multipanel_solution_constraint(solution_con,test_mstATA))

  test_mstATA2 <- mst_design(itempool = reading_itempool,
                             design = "1-3-3",module_length = c(14,12,12,12,6,6,6),
                             pivot_stim_map = pivot_stim_map)
  solution_con2<-combine_constraints(list(solution_itemcount_con(x = test_mstATA,">=",40),
                                          solution_itemcount_con(x = test_mstATA,"<=",50)))
  expect_true(is_multipanel_solution_constraint(solution_con2,test_mstATA2))
})
