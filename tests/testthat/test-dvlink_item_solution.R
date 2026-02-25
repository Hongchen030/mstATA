test_that("dvlink_item_solution basic correctness", {
  # MST 1-3-3
  x <- make_test_mstATA_BU()
  PoolSize<-nrow(x$ItemPool)
  num_panels<-2
  NumModules<-x$NumModules
  max_use<-NumModules*num_panels
  out <- dvlink_item_solution(x = x,num_panels = num_panels,
                              global_min_use = 0,global_max_use = max_use)
  item_module_panel<-PoolSize*NumModules
  expect_s3_class(out, "mstATA_constraint")
  expect_true(is(out$A_binary, "dgCMatrix"))
  PoolSize<-nrow(test_itempool)
  n_rows_expected <- PoolSize+ PoolSize
  expect_equal(nrow(out$A_binary), n_rows_expected)

  for (i in 1:PoolSize) {
    expect_equal(as.vector(out$A_binary[i, (item_module_panel*num_panels+i)]), -1)
  }

  for (i in 1:PoolSize) {
    row <- PoolSize + i
    expect_equal(as.vector(out$A_binary[row, (item_module_panel*num_panels+i)]), -max_use)
  }

  expect_equal(apply(out$A_binary[,1:(item_module_panel*num_panels)],1,sum),rep(NumModules*num_panels,PoolSize*2))

})
