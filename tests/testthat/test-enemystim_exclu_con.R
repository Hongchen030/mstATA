test_that("enemystim_exclu_con errors when x is not mstATA_design", {
  expect_error(
    enemystim_exclu_con(
      x = list()),
    "must be an object of class 'mstATA_design'"
  )
})


test_that("enemystim_exclu_con builds correct exclusion sets", {
  enemystim_set<-create_enemy_sets(test_itempool$stimulus,test_itempool$enemy_stimulus)
  pivot_stim_map<-create_pivot_stimulus_map(test_itempool,"item_id","stimulus","pivot_item")
  x<- mst_design(itempool = test_itempool,
                 design = "1-2",
                 module_length = c(2, 3, 3),
                 enemystim_set = enemystim_set,
                 pivot_stim_map = pivot_stim_map)
  res <- enemystim_exclu_con(x = x)
  NumPathways<-x$NumPathways
  NumStages<-2
  NumSets<-2
  Setsize<-c(2,3)
  expect_equal(nrow(res$A_binary),NumSets*NumPathways)
  expect_equal(res$operators, rep("<=",NumSets*NumPathways))
  expect_equal(res$d, rep(1,NumSets*NumPathways))
  expect_equal(apply(res$A_binary,1,sum),c(rep(2*NumStages,NumPathways),rep(3*NumStages,NumPathways)))
})

