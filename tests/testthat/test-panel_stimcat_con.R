test_that("panel_stimcat_con builds correct panel-level stimulus-category constraints", {

  pivot_stim_map<-create_pivot_stimulus_map(test_itempool,item_id_col = "item_id","stimulus","pivot_item")
  x <- mst_design(itempool = test_itempool,
                  design = "1-3-3",
                  module_length = c(4, 2, 2, 2, 2, 2, 2),
                  pivot_stim_map = pivot_stim_map)

  con <- panel_stimcat_con(
    x = x,
    attribute = "stimulus_type",
    cat_levels = c("history","social studies"),
    operator = ">=",
    target_num = c(2,1)
  )

  # Structure
  expect_s3_class(con, "mstATA_constraint")
  expect_equal(nrow(con$A_binary), 2)
  expect_equal(ncol(con$A_binary), length(x$decisionvar_name))

  expect_equal(apply(con$A_binary,1,sum), c(35, 35))
  expect_equal(con$d, c(2,1))
  expect_equal(con$operators, rep(check_operator(">="), 2))

  expect_equal(con$specification$Type, "Categorical")
  expect_equal(con$specification$`Application Level`, "Panel-level")

  # stim 1, stim 2, stim 5, stim 6, stim 7 are historical
  # pivot 26, 29, 55, 63, 70
  PoolSize<-nrow(test_itempool)
  col_offset<-(1:7-1L)*PoolSize
  which_history1<-(rep(c(26,29,55,63,70),each=7)+rep(col_offset,5))
  expect_equal(as.vector(as.matrix(con$A_binary[1,which_history1])),rep(1,35))

  which_social2<-rep(c(43,46,82,90,95),each=7)+rep(col_offset,5)
  expect_equal(as.vector(as.matrix(con$A_binary[2,which_social2])),rep(1,35))
})
