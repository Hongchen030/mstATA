test_that("stim_itemquant_con errors when x is not mstATA_design", {
  expect_error(
    stim_itemquant_con(
      x = list(),
      attribute = "time",
      operator = ">=",
      target_value = 100,
      which_module = 1
    ),
    "must be an object of class 'mstATA_design'"
  )
})


test_that("stim_itemquant_con errors when quantitative attribute has missing values", {

  itempool <- data.frame(
    item_id    = 1:4,
    stimulus   = c("S1", "S1", "S2", "S2"),
    pivot_flag = c(1, NA, 1, NA),
    time       = c(10, NA, 30, 40)
  )
  itempool$pivot_flag<-as.character(itempool$pivot_flag)
  pivot_stim_map=create_pivot_stimulus_map(itempool = itempool,stimulus = "stimulus",pivot_item = "pivot_flag")

  x_bad <- mst_design(itempool, design = "1-3", pathway_length = 10,
                      pivot_stim_map = pivot_stim_map)

  expect_error(
    stim_itemquant_con(
      x_bad,
      attribute = "time",
      operator = ">=",
      target_value = 50,
      which_module = 1
    ),
    "must not contain NA, NaN, or infinite values."
  )
})

test_that("stim_itemquant_con builds correct MODULE-level constraints", {

  itempool <- data.frame(
    item_id    = 1:5,
    stimulus   = c("S1", "S1", "S1", "S2", "S2"),
    pivot_flag = c(1, NA, NA, 1, NA),
    time       = c(10, 20, 30, 40, 20)
  )
  itempool$pivot_flag<-as.character(itempool$pivot_flag)
  pivot_stim_map=create_pivot_stimulus_map(itempool = itempool,stimulus = "stimulus",pivot_item = "pivot_flag")

  x <- mst_design(itempool, design = "1-3", pathway_length = 10,
                  pivot_stim_map = pivot_stim_map)

  res <- stim_itemquant_con(
    x,
    attribute   = "time",
    operator    = ">=",
    target_value = 60,
    which_module = 1
  )

  # Should produce one constraint per stimulus having pivot
  # Pivots exist for S1 and S2
  expect_equal(nrow(res$A_binary), 2)

  # Operators and RHS
  expect_equal(res$operators, c(">=", ">="))
  expect_equal(res$d, c(0, 0))
  expect_equal(apply(res$A_binary,1,sum),c(0,0))
  NumStimulus<-2
  pivot_item_ids<-x$pivot_stim_map$pivot_item_id
  itemsets<-x$pivot_stim_map$stimulus_members
  for(stim_id in seq_len(NumStimulus)){
    pivot<-pivot_item_ids[stim_id]
    non_pivot<-setdiff(itemsets[[stim_id]],pivot)
    expect_equal(as.numeric(res$A_binary[stim_id,pivot]),(itempool$time[pivot]-60))
    expect_equal(as.numeric(res$A_binary[stim_id,non_pivot]),itempool$time[non_pivot])
  }
})

