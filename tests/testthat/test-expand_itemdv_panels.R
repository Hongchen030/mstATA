test_that("expand_itemdv_panels correctly identifies x_im columns", {
  PoolSize<-100
  NumModules<-4
  x <- mst_design(itempool = data.frame(Item=1:PoolSize),item_id_col = "Item",
                  design = "1-3",pathway_length = 10)
  num_panels<-2
  # ---- Test expand_itemdv_panels for 2 panels ----
  item_cols <- expand_itemdv_panels(x,num_panels = num_panels)

  # Correct number of items
  expect_equal(length(item_cols), PoolSize)

  expect_equal(unlist(lapply(item_cols,length)), rep(NumModules*num_panels,PoolSize))

  # Check pattern for item 1
  expect_true(all(grep("^x\\[[0-9]+,1\\]$", x$decisionvar_name[(item_cols[[1]]%%NumModules*PoolSize)+1])))

  # Ensure all columns exist within expanded x_imp space
  base_cols <- grep("^x\\[[0-9]+,1\\]$", x$decisionvar_name)
  block_size <- length(x$decisionvar_name)

  # panel 1 block + panel 2 block
  expected_cols <- c(base_cols, base_cols + block_size)
  expected_cols <- sort(expected_cols)

  expect_equal(sort(item_cols[[1]]), expected_cols)

})

test_that("expand_itemdv_panels correctly identifies x_im columns", {
  item_module_eligibility<-list(`1`=1:nrow(mini_itempool),
                                `2`=which(mini_itempool$difficulty<=0),
                                `3`=intersect(which(mini_itempool$difficulty>=-1),which(mini_itempool$difficulty<1)),
                                `4`=which(mini_itempool$difficulty>=0),
                                `5`=intersect(which(mini_itempool$difficulty>=-2),which(mini_itempool$difficulty<0)),
                                `6`=intersect(which(mini_itempool$difficulty>=-1),which(mini_itempool$difficulty<1)),
                                `7`=intersect(which(mini_itempool$difficulty>=0),which(mini_itempool$difficulty<2)))
  PoolSize<-nrow(mini_itempool)
  NumModules<-7
  num_panels<-2
  x <- mst_design(itempool = mini_itempool,
                  design = "1-3-3",pathway_length = 10,
                  item_module_eligibility = item_module_eligibility)

  # ---- Test expand_itemdv_panels for 2 panels ----
  item_cols <- expand_itemdv_panels(x,num_panels = 2)

  item1<-names(unlist(lapply(item_module_eligibility,FUN = function(x) which(x==1))))
  item2<-names(unlist(lapply(item_module_eligibility,FUN = function(x) which(x==2))))
  item3<-names(unlist(lapply(item_module_eligibility,FUN = function(x) which(x==3))))
  expect_equal(length(item_cols[[1]]), length(item1)*num_panels)
  expect_equal(length(item_cols[[2]]), length(item2)*num_panels)
  expect_equal(length(item_cols[[3]]), length(item3)*num_panels)




  item_module_eligibility<-list(`1`=1:nrow(mini_itempool),
                                `2`=which(mini_itempool$difficulty<=0),
                                `4`=which(mini_itempool$difficulty>=0),
                                `7`=intersect(which(mini_itempool$difficulty>=0),which(mini_itempool$difficulty<2)))
  PoolSize<-nrow(mini_itempool)
  NumModules<-7
  num_panels<-2
  x <- mst_design(itempool = mini_itempool,
                  design = "1-3-3",pathway_length = 10,
                  item_module_eligibility = item_module_eligibility)
  item_module_eligibility_new<-check_item_module_eligibility(item_module_eligibility,
                                                             setNames(seq(nrow(mini_itempool)),mini_itempool$item_id),
                                                             NumModules = 7)
  # ---- Test expand_itemdv_panels for 2 panels ----
  item_cols <- expand_itemdv_panels(x,num_panels = 2)

  item1<-names(unlist(lapply(item_module_eligibility_new,FUN = function(x) which(x==1))))
  item2<-names(unlist(lapply(item_module_eligibility_new,FUN = function(x) which(x==2))))
  item3<-names(unlist(lapply(item_module_eligibility_new,FUN = function(x) which(x==3))))
  expect_equal(length(item_cols[[1]]), length(item1)*num_panels)
  expect_equal(length(item_cols[[2]]), length(item2)*num_panels)
  expect_equal(length(item_cols[[3]]), length(item3)*num_panels)
})
