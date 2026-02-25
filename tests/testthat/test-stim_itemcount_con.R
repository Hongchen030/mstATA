test_that("stim_itemcount_con errors when x is not mstATA_design", {
  expect_error(
    stim_itemcount_con(
      x = list(),
      min = 2,max = NULL,
      which_module = 1
    ),
    "must be an object of class 'mstATA_design'"
  )
})


test_that("stim_itemcount_con builds correct module-level constraints if num of stimuli in specific modules", {
  # mst 1-3-3
  x<-make_test_mstATA_BU()
  res <- stim_itemcount_con(
    x,
    min = 2,max = 5,
    which_module = 1
  )
  NumModules<-x$NumModules
  test_itempool$stimulus<-factor(test_itempool$stimulus,levels = paste("sti",1:10),labels = paste("sti",1:10))
  NumStimulus<-nlevels(test_itempool$stimulus)
  numItem_sti<-as.vector(table(test_itempool$stimulus))
  # correct operator
  expect_equal(res$operators, rep("<=",NumStimulus*2))

  # only stimulus with pivot (S1 and S2)
  expect_equal(nrow(res$A_binary), NumStimulus*2)
  # RHS = target_num
  expect_equal(res$d, rep(0,NumStimulus*2))
  min<-2
  max<-5
  expect_equal(all(apply(res$A_binary,1,sum)%in%c(min-numItem_sti,numItem_sti-max)),TRUE)
  pivot_item_id<-x$pivot_stim_map$pivot_item_id
  item_sets<-x$pivot_stim_map$stimulus_members
  # 1- max, min-1
  for(stim_id in seq_len(NumStimulus)){
    pivot<-pivot_item_id[stim_id]
    non_pivot<-setdiff(item_sets[[stim_id]],pivot)
    LB_row<-1+2L*(stim_id-1L)
    UB_row<-2L*stim_id
    expect_true(all(res$A_binary[LB_row,non_pivot]==-1))
    expect_true(all(res$A_binary[UB_row,non_pivot]==1))
    expect_true(res$A_binary[LB_row, pivot] == (min - 1))
    expect_true(res$A_binary[UB_row, pivot] == (1 - max))
  }
})

test_that("stim_itemcount_con builds correct module-level constraints if num of stimuli in specific modules (all in)", {
  # mst 1-3-3
  x<-make_test_mstATA_BU()
  res <- stim_itemcount_con(
    x,
    which_module = 1
  )
  NumModules<-x$NumModules
  test_itempool$stimulus<-factor(test_itempool$stimulus,levels = paste("sti",1:10),labels = paste("sti",1:10))
  NumStimulus<-nlevels(test_itempool$stimulus)
  numItem_sti<-as.vector(table(test_itempool$stimulus))
  # correct operator
  expect_equal(res$operators, rep("=",NumStimulus))

  expect_equal(nrow(res$A_binary), NumStimulus)
  # RHS = target_num
  expect_equal(res$d, rep(0,NumStimulus))
  expect_equal(apply(res$A_binary,1,sum),rep(0,NumStimulus))

  pivot_item_id<-x$pivot_stim_map$pivot_item_id
  item_sets<-x$pivot_stim_map$stimulus_members
  for(stim_id in seq_len(NumStimulus)){
    pivot<-pivot_item_id[stim_id]
    non_pivot<-setdiff(item_sets[[stim_id]],pivot)
    num_items<-length(item_sets[[stim_id]])
    expect_true(all(res$A_binary[stim_id,non_pivot]==1))
    expect_true(res$A_binary[stim_id,pivot]==(1-num_items))
  }
})

test_that("stim_itemcount_con builds correct module-level constraints if num of stimuli in specific pathways", {
  # mst 1-3-3
  x<-make_test_mstATA_BU()
  res <- stim_itemcount_con(
    x,
    min = 2,max = 5,
    which_pathway = c(1,5,9)
  )

  test_itempool$stimulus<-factor(test_itempool$stimulus,levels = paste("sti",1:10),labels = paste("sti",1:10))
  NumStimulus<-nlevels(test_itempool$stimulus)
  numItem_sti<-as.vector(table(test_itempool$stimulus))
  # correct operator
  expect_equal(res$operators, rep("<=",NumStimulus*2*7))

  # only stimulus with pivot (S1 and S2)
  expect_equal(nrow(res$A_binary), NumStimulus*2*7)
  # RHS = target_num
  expect_equal(res$d, rep(0,NumStimulus*2*7))
  min<-2
  max<-5
  expect_equal(all(apply(res$A_binary,1,sum)%in%c(min-numItem_sti,numItem_sti-max)),TRUE)
})
