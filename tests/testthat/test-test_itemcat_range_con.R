test_that("test_itemcat_range_con: single max, change to test_itemcat_con", {
  x<-make_test_mstATA_BU()

  out<-test_itemcat_range_con(
    x = x,
    attribute = "content",
    cat_levels = "content1",
    min = NULL, max = 2,
    which_module = 1
  )
  expect_equal(out$operator,"<=")
})


test_that("range constraint with explicit min/max yields valid constraint object", {
  ### Itempool size 100, MST 1-3-3,
  ### Content1-4: 20,30,25,25
  ### item type: CR,MC,TEI-10,61,29
  x<-make_test_mstATA_BU()

  # ----- Call function -----
  out <- test_itemcat_range_con(
    x = x,
    attribute = "content",
    cat_levels = c("content1", "content2"),
    min = c(1,3),max = c(5,6),
    which_module = 2:4
  )

  # ----- Check -----
  expect_true(
    inherits(out, "mstATA_constraint"),
    info = "The returned object must inherit class 'mstATA_constraint'"
  )
  expect_equal(length(out$operators), nrow(out$A_binary))
  expect_equal(ncol(out$A_binary),700)
  expect_equal(nrow(out$A_binary),12)
  expect_equal(out$operators,c(rep(">=",6),rep("<=",6)))
  expect_equal(out$d,c(rep(1,3),rep(3,3),rep(5,3),rep(6,3)))
  expect_equal(apply(out$A_binary,1,sum),rep(c(rep(20,3),rep(30,3)),2))
})


test_that("range constraint computes min/max from target ± deviation correctly", {
  x<-make_test_mstATA_BU()
  out <- test_itemcat_range_con(
    x = x,
    attribute = "content",
    cat_levels = c("content1","content2"),
    target = c(5, 3),
    deviation = c(2, 1),
    which_module = 2:4
  )

  expect_equal(out$d,c(rep(c(3,2),each=3),rep(c(7,4),each=3)))
})



test_that("pathway-level category constraint builds correct structure,min,max", {
  x<-make_test_mstATA_TD()

  out <- test_itemcat_range_con(
    x = x,
    attribute = "content",
    cat_levels = c("content1", "content2"),
    min = c(1,3),max = c(5,6),
    which_pathway = 1:2
  )

  expect_true(
    inherits(out, "mstATA_constraint"),
    info = "The returned object must inherit class 'mstATA_constraint'"
  )
  expect_equal(length(out$operators), nrow(out$A_binary))
  expect_equal(ncol(out$A_binary),700)
  expect_equal(nrow(out$A_binary),8)
  expect_equal(out$operators,rep(c(">=","<="),each=4))
  expect_equal(out$d,c(1,1,3,3,5,5,6,6))
  expect_equal(apply(out$A_binary,1,sum),rep(c(rep(60,2),rep(90,2)),2))
})

test_that("pathway-level category constraint builds correct structure,target,deviation", {
  x<-make_test_mstATA_TD()

  out <- test_itemcat_range_con(
    x = x,
    attribute = "content",
    cat_levels = c("content1", "content2"),
    target = c(5, 3),
    deviation = c(2, 1),
    which_pathway = 1:2
  )

  expect_true(
    inherits(out, "mstATA_constraint"),
    info = "The returned object must inherit class 'mstATA_constraint'"
  )
  expect_equal(length(out$operators), nrow(out$A_binary))
  expect_equal(ncol(out$A_binary),700)
  expect_equal(nrow(out$A_binary),8)
  expect_equal(out$operators,rep(c(">=","<="),each=4))
  expect_equal(out$d,c(3,3,2,2,7,7,4,4))
  expect_equal(apply(out$A_binary,1,sum),rep(c(rep(60,2),rep(90,2)),2))
})


test_that("test_itemcat_range_con: single max, change to test_itemcat_con", {
  content_levels<-paste0("content",1:4)
  content<-sample(content_levels,size = 100,replace = TRUE)
  target_num<-c(1,2,3,4)
  itempool<-data.frame(item_id=paste0("Item",1:100),
                       content=content)
  x<-mst_design(itempool = itempool,design = "1-3",module_length = c(3,4,4,4))


  out<-test_itemcat_range_con(
    x = x,
    attribute = "content",
    cat_levels = "content1",
    min = NULL, max = 2,
    which_module = 1
  )
  which_content1<-which(content=="content1")
  expect_equal(out$operator,"<=")
  expect_equal(out$d,2)
  expect_equal(sum(as.vector(out$A_binary[1,1:100])),length(which_content1))


  out<-test_itemcat_range_con(
    x = x,
    attribute = "content",
    cat_levels = "content1",
    min = NULL, max = 2,
    which_pathway = 1
  )
  NumStages<-x$NumStages
  expect_equal(sum(as.vector(out$A_binary[1,1:200])),length(which_content1)*NumStages)
  expect_equal(sum(as.vector(out$A_binary[1,201:400])),0)
})


test_that("range constraint with explicit min/max yields valid constraint object", {
  content_levels<-paste0("content",1:4)
  content<-sample(content_levels,size = 100,replace = TRUE)
  target_num<-c(1,2,3,4)
  itempool<-data.frame(item_id=paste0("Item",1:100),
                       content=content)
  x<-mst_design(itempool = itempool,design = "1-3",module_length = c(3,4,4,4))
  min<-c(1,3)
  max<-c(5,6)
  # ----- Call function -----
  out <- test_itemcat_range_con(
    x = x,
    attribute = "content",
    cat_levels = c("content1", "content2"),
    min = min,max = max,
    which_module = 2:4
  )
  NumModules<-x$NumModules
  PoolSize<-nrow(x$ItemPool)
  NumCategory<-2
  n_modules<-length(2:4)
  side<-2
  expect_equal(length(out$operators), nrow(out$A_binary))
  expect_equal(ncol(out$A_binary),NumModules*PoolSize)
  expect_equal(nrow(out$A_binary),n_modules*NumCategory*side)

  for(category_id in 1:2){
    which_content<-which(content==paste0("content",category_id))
    min_cat_start<-1+n_modules*(category_id-1)
    min_cat_end<-n_modules*category_id
    max_cat_start<-1+n_modules*NumCategory+n_modules*(category_id-1)
    max_cat_end<-max_cat_start+n_modules-1
    min_rhs<-min[category_id]
    max_rhs<-max[category_id]
    expect_equal(apply(out$A_binary[min_cat_start:min_cat_end,],1,sum),rep(length(which_content),n_modules))
    expect_equal(apply(out$A_binary[max_cat_start:max_cat_end,],1,sum),rep(length(which_content),n_modules))

    expect_equal(out$d[min_cat_start:min_cat_end],rep(min_rhs,n_modules))
    expect_equal(out$d[max_cat_start:max_cat_end],rep(max_rhs,n_modules))

    expect_equal(out$operators[min_cat_start:min_cat_end],rep(">=",n_modules))
    expect_equal(out$operators[max_cat_start:max_cat_end],rep("<=",n_modules))
  }


  # ----- Call function -----
  out <- test_itemcat_range_con(
    x = x,
    attribute = "content",
    cat_levels = c("content1", "content2"),
    min = min,max = max,
    which_pathway = 2:3
  )
  NumStages<-x$NumStages
  n_pathways<-length(2:3)
  for(category_id in 1:2){
    which_content<-which(content==paste0("content",category_id))
    min_cat_start<-1+n_pathways*(category_id-1)
    min_cat_end<-n_pathways*category_id
    max_cat_start<-1+n_pathways*NumCategory+n_pathways*(category_id-1)
    max_cat_end<-max_cat_start+n_pathways-1
    min_rhs<-min[category_id]
    max_rhs<-max[category_id]
    expect_equal(apply(out$A_binary[min_cat_start:min_cat_end,],1,sum),rep(length(which_content)*NumStages,n_pathways))
    expect_equal(apply(out$A_binary[max_cat_start:max_cat_end,],1,sum),rep(length(which_content)*NumStages,n_pathways))

    expect_equal(out$d[min_cat_start:min_cat_end],rep(min_rhs,n_pathways))
    expect_equal(out$d[max_cat_start:max_cat_end],rep(max_rhs,n_pathways))

    expect_equal(out$operators[min_cat_start:min_cat_end],rep(">=",n_pathways))
    expect_equal(out$operators[max_cat_start:max_cat_end],rep("<=",n_pathways))
  }

})


test_that("range constraint computes min/max from target ± deviation correctly", {
  content_levels<-paste0("content",1:4)
  content<-sample(content_levels,size = 100,replace = TRUE)
  target_num<-c(1,2,3,4)
  itempool<-data.frame(item_id=paste0("Item",1:100),
                       content=content)
  x<-mst_design(itempool = itempool,design = "1-3",module_length = c(3,4,4,4))

  ## length of target = num of constrained categories
  target<-c(5,3)
  deviation<-c(2,1)
  out <- test_itemcat_range_con(
    x = x,
    attribute = "content",
    cat_levels = c("content1","content2"),
    target = target,
    deviation = deviation,
    which_module = 2:4
  )

  NumModules<-x$NumModules
  PoolSize<-nrow(x$ItemPool)
  NumCategory<-2
  n_modules<-length(2:4)
  side<-2
  expect_equal(length(out$operators), nrow(out$A_binary))
  expect_equal(ncol(out$A_binary),NumModules*PoolSize)
  expect_equal(nrow(out$A_binary),n_modules*NumCategory*side)
  exp_min_max<-check_target_bounds(levels = c("content1","content2"),target = target,deviation = deviation,total_rows = 4,row_ids = 2:4)
  min<-exp_min_max[["min"]]
  max<-exp_min_max[["max"]]

  for(category_id in 1:2){
    which_content<-which(content==paste0("content",category_id))
    min_cat_start<-1+n_modules*(category_id-1)
    min_cat_end<-n_modules*category_id
    max_cat_start<-1+n_modules*NumCategory+n_modules*(category_id-1)
    max_cat_end<-max_cat_start+n_modules-1
    min_rhs<-as.vector(min[,category_id])
    max_rhs<-as.vector(max[,category_id])
    expect_equal(apply(out$A_binary[min_cat_start:min_cat_end,],1,sum),rep(length(which_content),n_modules))
    expect_equal(apply(out$A_binary[max_cat_start:max_cat_end,],1,sum),rep(length(which_content),n_modules))

    expect_equal(out$d[min_cat_start:min_cat_end],min_rhs)
    expect_equal(out$d[max_cat_start:max_cat_end],max_rhs)

    expect_equal(out$operators[min_cat_start:min_cat_end],rep(">=",n_modules))
    expect_equal(out$operators[max_cat_start:max_cat_end],rep("<=",n_modules))
  }


  # ----- Call function -----
  out <- test_itemcat_range_con(
    x = x,
    attribute = "content",
    cat_levels = c("content1", "content2"),
    target = target,deviation = deviation,
    which_pathway = 2:3
  )
  NumStages<-x$NumStages
  n_pathways<-length(2:3)
  exp_min_max<-check_target_bounds(levels = c("content1","content2"),target = target,deviation = deviation,total_rows = 2,row_ids = 2:3)
  min<-exp_min_max[["min"]]
  max<-exp_min_max[["max"]]
  for(category_id in 1:2){
    which_content<-which(content==paste0("content",category_id))
    min_cat_start<-1+n_pathways*(category_id-1)
    min_cat_end<-n_pathways*category_id
    max_cat_start<-1+n_pathways*NumCategory+n_pathways*(category_id-1)
    max_cat_end<-max_cat_start+n_pathways-1
    min_rhs<-as.vector(min[,category_id])
    max_rhs<-as.vector(max[,category_id])
    expect_equal(apply(out$A_binary[min_cat_start:min_cat_end,],1,sum),rep(length(which_content)*NumStages,n_pathways))
    expect_equal(apply(out$A_binary[max_cat_start:max_cat_end,],1,sum),rep(length(which_content)*NumStages,n_pathways))

    expect_equal(out$d[min_cat_start:min_cat_end],min_rhs)
    expect_equal(out$d[max_cat_start:max_cat_end],max_rhs)

    expect_equal(out$operators[min_cat_start:min_cat_end],rep(">=",n_pathways))
    expect_equal(out$operators[max_cat_start:max_cat_end],rep("<=",n_pathways))
  }


  ## length of target = num of modules
  target<-c(5,3,2)
  deviation<-1
  out <- test_itemcat_range_con(
    x = x,
    attribute = "content",
    cat_levels = c("content1","content2"),
    target = target,
    deviation = deviation,
    which_module = 2:4
  )

  NumModules<-x$NumModules
  PoolSize<-nrow(x$ItemPool)
  NumCategory<-2
  n_modules<-length(2:4)
  side<-2
  expect_equal(length(out$operators), nrow(out$A_binary))
  expect_equal(ncol(out$A_binary),NumModules*PoolSize)
  expect_equal(nrow(out$A_binary),n_modules*NumCategory*side)
  exp_min_max<-check_target_bounds(levels = c("content1","content2"),target = target,deviation = deviation,total_rows = 4,row_ids = 2:4)
  min<-exp_min_max[["min"]]
  max<-exp_min_max[["max"]]

  for(category_id in 1:2){
    which_content<-which(content==paste0("content",category_id))
    min_cat_start<-1+n_modules*(category_id-1)
    min_cat_end<-n_modules*category_id
    max_cat_start<-1+n_modules*NumCategory+n_modules*(category_id-1)
    max_cat_end<-max_cat_start+n_modules-1
    min_rhs<-as.vector(min[,category_id])
    max_rhs<-as.vector(max[,category_id])
    expect_equal(apply(out$A_binary[min_cat_start:min_cat_end,],1,sum),rep(length(which_content),n_modules))
    expect_equal(apply(out$A_binary[max_cat_start:max_cat_end,],1,sum),rep(length(which_content),n_modules))

    expect_equal(out$d[min_cat_start:min_cat_end],min_rhs)
    expect_equal(out$d[max_cat_start:max_cat_end],max_rhs)

    expect_equal(out$operators[min_cat_start:min_cat_end],rep(">=",n_modules))
    expect_equal(out$operators[max_cat_start:max_cat_end],rep("<=",n_modules))
  }
})
