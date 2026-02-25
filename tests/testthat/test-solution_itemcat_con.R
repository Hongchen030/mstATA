test_that("solution_itemcat_con builds correct category constraints", {

  x <- mst_design(mini_itempool,design = "1-3",
                  module_length = c(3,4,4,4))
  PoolSize<-nrow(x$ItemPool)
  total_cols <- PoolSize

  levels <- c("content1","content3")
  target <- c(15,20)

  con <- solution_itemcat_con(
    x,
    attribute = "content",
    cat_levels = levels,
    operator = ">=",
    target_num = target
  )

  expect_s3_class(con, "mstATA_constraint")
  expect_equal(nrow(con$A_binary), 2)      # two category levels
  expect_equal(ncol(con$A_binary), total_cols)

  s_cols <- seq_len(PoolSize)
  num_content1<-sum(mini_itempool$content=="content1")
  num_content3<-sum(mini_itempool$content=="content3")
  which_content1<-which(mini_itempool$content=="content1")
  which_content3<-which(mini_itempool$content=="content3")
  expect_equal(apply(con$A_binary,1,sum),c(num_content1,num_content3))
  expect_equal(apply(con$A_binary[,s_cols],1,sum),c(num_content1,num_content3))
  expect_equal(as.vector(unlist(apply(con$A_binary,1,FUN = function(x) which(x==1)))),c(which_content1,which_content3))
  expect_equal(con$d, target)

  # invalid case: no such category
  expect_error(
    solution_itemcat_con(
      x, num_panels, attribute="content",
      cat_levels="Z", operator=">=", target_num=1
    )
  )
})
