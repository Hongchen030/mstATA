test_that("needs_dvlink_item_stim detects module-level logical constraints", {

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
  itemset_con2<-stim_itemquant_con(x = test_mstATA,attribute = "time",operator = ">=",
                                   target_value = 100,
                                   which_module = 1,which_pathway = NULL)
  itemset_con3<-stim_itemcat_con(x = test_mstATA,attribute = "itemtype",
                                 cat_levels = c("MC","TEI"), min = c(2,1))
  expect_false(needs_dvlink_item_stim(list(discrete_con)))
  expect_false(needs_dvlink_item_stim(list(discrete_con,stimulus_con)))
  expect_false(needs_dvlink_item_stim(list(discrete_con,stimulus_con,itemset_con)))
  expect_true(needs_dvlink_item_stim(list(itemset_con2)))
  expect_false(needs_dvlink_item_stim(list(itemset_con,itemset_con2)))
  expect_false(needs_dvlink_item_stim(list(itemset_con,itemset_con3)))
})
