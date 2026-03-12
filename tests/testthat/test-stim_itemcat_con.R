test_that("stim_itemcat_con errors when x is not mstATA_design", {
  expect_error(
    stim_itemcat_con(
      x = list(),
      attribute = "content",
      cat_levels = c("A", "B"),
      operator="=",target_num=1,
      which_module = 1
    ),
    "must be an object of class 'mstATA_design'"
  )
})

test_that("stim_itemcat_con builds constraints with correct dimensions", {
  # Simple toy pool
  itempool <- data.frame(
    item_id    = 1:5,
    stimulus   = c("S1", "S1", "S1", "S2", "S2"),
    pivot_flag = c(1, NA, NA, 1, NA),
    content    = c("A", "A", "B", "A", "B"),
    stringsAsFactors = FALSE
  )
  itempool$pivot_flag<-as.character(itempool$pivot_flag)
  pivot_stim_map<-create_pivot_stimulus_map(itempool = itempool,item_id_col = "item_id",stimulus = "stimulus",pivot_item = "pivot_flag")
  x <- mst_design(
    itempool = itempool,
    design = "1-3",
    pathway_length = 10,
    pivot_stim_map = pivot_stim_map
  )
  min<-1
  max<-2
  res <- combine_constraints(list(stim_itemcat_con(x = x,
                                                   attribute = "content",
                                                   cat_levels = c("A", "B"),
                                                   operator = ">=",target_num = 1,
                                                   which_module = 1),
                                  stim_itemcat_con(x = x,
                                                   attribute = "content",
                                                   cat_levels = c("A", "B"),
                                                   operator = "<=",target_num = 2,
                                                   which_module = 1)))

  NumStimulus <- length(unique(na.omit(itempool$stimulus)))   # S1, S2
  NumCategory <- 2                                            # A, B
  n_modules   <- 1
  pivot_item_ids<-x$pivot_stim_map$pivot_item_id
  itemsets<-x$pivot_stim_map$stimulus_members
  # stim 1: row 1,3,5,7

  expect_equal(as.vector(res$A_binary[1,c(1,2,3)]),c(0,1,0))
  expect_equal(as.vector(res$A_binary[3,c(1,2,3)]),c(-1,0,1))
  expect_equal(as.vector(res$A_binary[5,c(1,2,3)]),c(-1,1,0))
  expect_equal(as.vector(res$A_binary[7,c(1,2,3)]),c(-2,0,1))

  expect_equal(as.vector(res$A_binary[2,c(4,5)]),c(0,0))
  expect_equal(as.vector(res$A_binary[4,c(4,5)]),c(-1,1))
  expect_equal(as.vector(res$A_binary[6,c(4,5)]),c(-1,0))
  expect_equal(as.vector(res$A_binary[8,c(4,5)]),c(-2,1))

  expect_equal(
    nrow(res$A_binary),
    NumStimulus * NumCategory * n_modules*2
  )

  expect_equal(
    ncol(res$A_binary),
    x$NumModules * nrow(x$ItemPool)
  )

  # operators length matches rows
  expect_equal(length(res$operators), nrow(res$A_binary))

  # RHS all zeros
  expect_true(all(res$d == 0))

  # Specification matches
  spec <- res$specification
  expect_equal(unique(spec$`Application Level`), "Module-level")
  expect_equal(unique(spec$Type), "Logical")
  expect_equal(sum(spec$`Num of Constraints`), nrow(res$A_binary))
})

test_that("stim_itemcat_con works when pivot is in the category", {
  itempool <- data.frame(
    item_id    = 1:10,
    stimulus   = c(rep("S1",6),rep(NA,4)),
    pivot_flag = c(1, rep(NA,9)),
    content    = c(c("A", "A", "B","B","A","B"),c("B","A","B","A")),
    stringsAsFactors = FALSE
  )
  itempool$pivot_flag<-as.character(itempool$pivot_flag)
  pivot_stim_map<-create_pivot_stimulus_map(itempool = itempool,stimulus = "stimulus",pivot_item = "pivot_flag")
  x <- mst_design(
    itempool = itempool,
    design = "1-3",
    pathway_length = 5,
    pivot_stim_map = pivot_stim_map
  )

  res <- combine_constraints(list(stim_itemcat_con(x = x,
                                                   attribute = "content",
                                                   cat_levels = c("A", "B"),
                                                   operator = ">=",target_num = 1,
                                                   which_module = 1),
                                  stim_itemcat_con(x = x,
                                                   attribute = "content",
                                                   cat_levels = c("A", "B"),
                                                   operator = "<=",target_num = 2,
                                                   which_module = 1)))

  # Just check it runs and structure is consistent
  expect_s4_class(res$A_binary, "dgCMatrix")
  expect_equal(length(res$operators), nrow(res$A_binary))
  expect_equal(apply(res$A_binary,1,sum),c(2,2,1,1))
})

test_that("stim_itemcat_con works when pivot is not in the category", {
  itempool <- data.frame(
    item_id    = 1:10,
    stimulus   = c(rep("S1",6),rep(NA,4)),
    pivot_flag = c(1, rep(NA,9)),
    content    = c(c("C", "A", "B","B","A","B"),c("B","A","B","A")),
    stringsAsFactors = FALSE
  )
  itempool$pivot_flag<-as.character(itempool$pivot_flag)
  pivot_stim_map<-create_pivot_stimulus_map(itempool = itempool,stimulus = "stimulus",pivot_item = "pivot_flag")
  x <- mst_design(
    itempool = itempool,
    design = "1-3",
    pathway_length = 5,
    pivot_stim_map = pivot_stim_map
  )
  res <- combine_constraints(list(stim_itemcat_con(x = x,
                                                   attribute = "content",
                                                   cat_levels = c("A", "B"),
                                                   operator = ">=",target_num = 1,
                                                   which_module = 1),
                                  stim_itemcat_con(x = x,
                                                   attribute = "content",
                                                   cat_levels = c("A", "B"),
                                                   operator = "<=",target_num = 2,
                                                   which_module = 1)))

  expect_s4_class(res$A_binary, "dgCMatrix")
  expect_equal(length(res$operators), nrow(res$A_binary))
  expect_equal(apply(res$A_binary,1,sum),c(1,2,0,1))
})

test_that("stim_itemcat_con works when no items are in the category", {
  itempool <- data.frame(
    item_id    = 1:10,
    stimulus   = c(rep("S1",6),rep(NA,4)),
    pivot_flag = c(1, rep(NA,9)),
    content    = c(c("A", "A", "B","B","A","B"),c("C","C","B","A")),
    stringsAsFactors = FALSE
  )
  itempool$pivot_flag<-as.character(itempool$pivot_flag)
  pivot_stim_map<-create_pivot_stimulus_map(itempool = itempool,stimulus = "stimulus",pivot_item = "pivot_flag")
  x <- mst_design(
    itempool = itempool,
    design = "1-3",
    pathway_length = 5,
    pivot_stim_map = pivot_stim_map
  )
  res <- combine_constraints(list(stim_itemcat_con(x = x,
                                                   attribute = "content",
                                                   cat_levels = "C",
                                                   operator = ">=",target_num = 1,
                                                   which_module = 1),
                                  stim_itemcat_con(x = x,
                                                   attribute = "content",
                                                   cat_levels = "C",
                                                   operator = "<=",target_num = 2,
                                                   which_module = 1)))

  expect_s4_class(res$A_binary, "dgCMatrix")
  expect_equal(length(res$operators), nrow(res$A_binary))
  expect_equal(length(res$operators),2)
  expect_equal(apply(res$A_binary,1,sum),c(-1,-2))
  expect_equal(res$operators,c(">=","<="))
  expect_equal(res$d,rep(0,2))
})


