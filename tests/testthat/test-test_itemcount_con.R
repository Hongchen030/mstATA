test_that("test_itemcount_con() works with ModuleLength", {
  ## MST 1-3-3, 4 items in stage 1, 2 items in stage 2, 2 items in stage 3
  x<-make_test_mstATA_BU()
  PoolSize<-nrow(x$ItemPool)
  NumModules<-x$NumModules

  constraint <- test_itemcount_con(x)

  expect_s3_class(constraint, "mstATA_constraint")
  expect_equal(nrow(constraint$A_binary), 7) # one per module
  expect_equal(length(constraint$d), 7)
  expect_true(all(constraint$d == c(4,2,2,2,2,2,2)))
  expect_equal(Matrix::rowSums(constraint$A_binary),rep(PoolSize,NumModules))
  expect_equal(constraint$operators,rep("=",NumModules))
  for(module_id in seq_len(NumModules)){
    expect_equal(which(as.vector(constraint$A_binary[module_id,])==1),seq_len(PoolSize)+PoolSize*(module_id-1L))
  }
  expect_equal(apply(constraint$A_binar,1,sum),rep(PoolSize,NumModules))
})


test_that("test_itemcount_con() works with PathwayLength", {
  ## MST 1-3-3, 8 items in each pathway, 1-1-3, 1-3-1 are excluded
  x<-make_test_mstATA_TD()
  PoolSize<-nrow(x$ItemPool)
  NumStages<-x$NumStages
  NumPathways<-x$NumPathways
  NumModules<-x$NumModules
  PathwayIndex<-x$PathwayIndex
  constraint <- test_itemcount_con(x)

  expect_s3_class(constraint, "mstATA_constraint")
  expect_equal(nrow(constraint$A_binary), NumPathways+NumModules+4)
  expect_equal(length(constraint$d), NumPathways+NumModules+4)
  expect_equal(constraint$d,c(rep(8,NumPathways),rep(1,NumModules),rep(0,4)))
  expect_equal(Matrix::rowSums(constraint$A_binary),c(rep(PoolSize*NumStages,NumPathways),rep(PoolSize,NumModules),rep(0,4)))

  for(row_id in seq_len(NumPathways)){
    pathway_id<-row_id
    involved_module<-as.integer(unlist(PathwayIndex[pathway_id, 1:NumStages]))
    expect_equal(which(as.vector(constraint$A_binary[pathway_id,])==1),
                 rep(seq_len(PoolSize),NumStages)+rep(PoolSize*(involved_module-1L),each=PoolSize))
    expect_equal(sum(as.vector(constraint$A_binary[row_id,])),PoolSize*NumStages)
    expect_equal(constraint$d[row_id],8)
  }
  for(row_id in (NumPathways+1):(NumPathways+NumModules)){
    expect_equal(which(as.vector(constraint$A_binary[row_id,])==1),
                 seq_len(PoolSize)+(row_id-NumPathways-1L)*PoolSize)
    expect_equal(sum(as.vector(constraint$A_binary[row_id,])),PoolSize)
    expect_equal(constraint$d[row_id],1)
  }
  for(row_id in (NumPathways+NumModules+1):nrow(constraint$A_binary)){
    expect_equal(sum(as.vector(constraint$A_binary[row_id,])),0)
    expect_equal(constraint$d[row_id],0)
  }

})

test_that("test_itemcount_con() works with PathwayLength,stage_length_bound is provided", {
  ## MST 1-3-3, 8 items in each pathway, 1-1-3, 1-3-1 are excluded
  x<-make_test_mstATA_TD()
  PoolSize<-nrow(x$ItemPool)
  NumStages<-x$NumStages
  NumPathways<-x$NumPathways
  NumModules<-x$NumModules
  stage_length_bound<-data.frame(stage = 1:3, min = c(1,1,1),max=c(4,NA,NA))
  constraint <- test_itemcount_con(x,stage_length_bound = stage_length_bound)

  expect_s3_class(constraint, "mstATA_constraint")
  expect_equal(nrow(constraint$A_binary), NumPathways+NumModules+1+4)
  expect_equal(length(constraint$d), NumPathways+NumModules+1+4)
  expect_equal(constraint$d,c(rep(8,NumPathways),rep(1,NumModules),4,rep(0,4)))
  expect_equal(Matrix::rowSums(constraint$A_binary),c(rep(PoolSize*NumStages,NumPathways),rep(PoolSize,NumModules+1),rep(0,4)))
})
