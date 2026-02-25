test_that("test_itemcat_con errors for non-mstATA_design input", {
  expect_error(
    test_itemcat_con(
      x = list(), attribute = "content",
      cat_levels = "A", operator = ">=", target_num = 1
    ),
    "must be an object of class 'mstATA_design'"
  )
})


test_that("module-level item category constraint builds correct structure", {
  ### Itempool size 100, MST 1-3-3,
  ### Content1-4: 20,30,25,25
  ### item type: CR,MC,TEI-10,61,29
  x<-make_test_mstATA_BU()

  # ----- Call function -----
  out <- test_itemcat_con(
    x = x,
    attribute = "content",
    cat_levels = c("content1", "content2"),
    operator = ">=",
    target_num = c(1, 3)
  )

  # ----- Check -----
  expect_true(
    inherits(out, "mstATA_constraint"),
    info = "The returned object must inherit class 'mstATA_constraint'"
  )
  expect_equal(length(out$operators), nrow(out$A_binary))
  expect_equal(ncol(out$A_binary),700)
  expect_equal(nrow(out$A_binary),14)
  expect_equal(out$operators,rep(">=",14))
  expect_equal(out$d,rep(c(1,3),each=7))
  expect_equal(apply(out$A_binary,1,sum),rep(c(20,30),each=7))
})

test_that("pathway-level category constraint builds correct structure", {
  x<-make_test_mstATA_TD()

  out <- test_itemcat_con(
    x = x,
    attribute = "content",
    cat_levels = c("content1", "content2"),
    operator = ">=",
    target_num = c(1, 3),
    which_pathway = 1
  )

  expect_true(
    inherits(out, "mstATA_constraint"),
    info = "The returned object must inherit class 'mstATA_constraint'"
  )
  expect_equal(length(out$operators), nrow(out$A_binary))
  expect_equal(ncol(out$A_binary),700)
  expect_equal(nrow(out$A_binary),2)
  expect_equal(out$operators,rep(">=",2))
  expect_equal(out$d,c(1,3))
  expect_equal(apply(out$A_binary,1,sum),c(60,90))
})

test_that("invalid attribute level produces error", {

  x <- make_test_mstATA_BU()

  expect_error(
    test_itemcat_con(
      x = x,
      attribute = "content",
      cat_levels = "NONEXISTENT",
      operator = ">=",
      target_num = 1
    ),
    sprintf("'cat_level' (%s) is not present in attribute '%s'.",
            "NONEXISTENT", "content"),
    fixed = TRUE
  )
})


test_that("module-level item category constraint builds correct structure", {
  content_levels<-paste0("content",1:4)
  content<-sample(content_levels,size = 100,replace = TRUE)
  target_num<-c(1,2,3,4)
  itempool<-data.frame(item_id=paste0("Item",1:100),
                       content=content)
  x<-mst_design(itempool = itempool,design = "1-3",module_length = c(3,4,4,4))

  # ----- Call function -----
  out <- test_itemcat_con(
    x = x,
    attribute = "content",
    cat_levels = content_levels,
    operator = ">=",
    target_num = target_num
  )
  NumModules<-x$NumModules
  PoolSize<-nrow(x$ItemPool)
  expect_equal(length(out$operators), nrow(out$A_binary))
  expect_equal(ncol(out$A_binary),NumModules*PoolSize)
  expect_equal(nrow(out$A_binary),NumModules*4)
  expect_equal(out$operators,rep(">=",NumModules*4))
  expect_equal(out$d,rep(c(1,2,3,4),each=NumModules))

  for(category_id in 1:4){
    which_content<-which(content==paste0("content",category_id))
    cat_start<-1+NumModules*(category_id-1)
    cat_end<-NumModules*category_id
    rhs<-target_num[category_id]
    expect_equal(apply(out$A_binary[cat_start:cat_end,],1,sum),rep(length(which_content),NumModules))
    expect_equal(out$d[cat_start:cat_end],rep(rhs,NumModules))
    for(module_id in 1:NumModules){
      which_col<-(1+(module_id-1)*PoolSize):(PoolSize*module_id)
      expect_equal(sum(as.vector(out$A_binary[cat_start+module_id-1,which_col])),length(which_content))
    }
  }
})

test_that("pathway-level item category constraint builds correct structure", {
  content_levels<-paste0("content",1:4)
  content<-sample(content_levels,size = 100,replace = TRUE)
  target_num<-c(1,2,3,4)
  itempool<-data.frame(item_id=paste0("Item",1:100),
                       content=content)
  x<-mst_design(itempool = itempool,design = "1-3",module_length = c(3,4,4,4))

  # ----- Call function -----
  out <- test_itemcat_con(
    x = x,
    attribute = "content",
    cat_levels = content_levels,
    operator = ">=",
    target_num = target_num,
    which_pathway = 1:3
  )
  NumModules<-x$NumModules
  NumPathways<-x$NumPathways
  NumStages<-x$NumStages
  PathwayIndex<-x$PathwayIndex
  PoolSize<-nrow(x$ItemPool)
  expect_equal(length(out$operators), nrow(out$A_binary))
  expect_equal(ncol(out$A_binary),NumModules*PoolSize)
  expect_equal(nrow(out$A_binary),NumPathways*4)
  expect_equal(out$operators,rep(">=",NumPathways*4))
  expect_equal(out$d,rep(c(1,2,3,4),each=NumPathways))

  for(category_id in 1:4){
    which_content<-which(content==paste0("content",category_id))
    cat_start<-1+NumPathways*(category_id-1)
    cat_end<-NumPathways*category_id
    rhs<-target_num[category_id]
    expect_equal(apply(out$A_binary[cat_start:cat_end,],1,sum),rep(length(which_content)*NumStages,NumPathways))
    expect_equal(out$d[cat_start:cat_end],rep(rhs,NumPathways))
    for(pathway_id in 1:NumPathways){
      involved_modules<-as.vector(as.matrix((PathwayIndex[PathwayIndex$pathway_index==pathway_id,paste0("stage",1:NumStages)])))
      which_col<-unlist(lapply(involved_modules,FUN = function(module_id) (1+(module_id-1)*PoolSize):(module_id*PoolSize)))
      expect_equal(sum(as.vector(out$A_binary[cat_start+pathway_id-1,which_col])),length(which_content)*NumStages)
    }
  }
})

