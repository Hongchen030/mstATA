test_that("solution_stimcat_con builds correct categorical stimulus constraints", {
  data("reading_itempool")
  pivot_stim_map<- create_pivot_stimulus_map(itempool = reading_itempool,stimulus="stimulus",pivot_item = "pivot_item")
  x <- mst_design(itempool = reading_itempool,design = "1-3",pathway_length = 10,
                  pivot_stim_map = pivot_stim_map)
  target<-c(3,4)
  con <- solution_stimcat_con(
    x,
    attribute = "stimulus_type",
    cat_levels = c("history","social studies"),
    operator = ">=",
    target_num = target
  )

  expect_s3_class(con, "mstATA_constraint")
  expect_equal(nrow(con$A_binary), 2)
  expect_equal(ncol(con$A_binary), nrow(reading_itempool))
  expect_equal(con$operators, c(">=", ">="))
  expect_equal(con$d, target)

  s_cols <- seq_len(nrow(reading_itempool))
  pivot_stim_map<-x$pivot_stim_map
  pivot_items<-pivot_stim_map$pivot_item_id
  stimulus_cat<-reading_itempool[pivot_items,"stimulus_type"]
  which_history<-pivot_items[which(stimulus_cat=="history")]
  which_social<-pivot_items[which(stimulus_cat=="social studies")]
  expect_equal(as.vector(con$A_binary[1, s_cols[which_history]]), rep(1,length(which_history)))
  expect_equal(as.vector(con$A_binary[2, s_cols[which_social]]),rep(1,length(which_social)))
  expect_equal(apply(con$A_binary,1,sum),c(length(which_history),length(which_social)))

  # invalid category
  expect_error(
    solution_stimcat_con(
      attribute = "stimulus_type",
      cat_levels = "ZZ",
      operator = ">=",
      target_num = 1
    )
  )
})
