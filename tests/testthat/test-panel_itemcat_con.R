test_that("panel_itemcat_con builds panel-level categorical constraints correctly", {

  # --- Fake mstATA object ---------------------------------------------------
  ItemPool <- data.frame(
    item_id = 1:4,
    content = c("A", "A", "B", "B"),
    stringsAsFactors = FALSE
  )

  PoolSize   <- nrow(ItemPool)
  NumModules <- 2L

  decisionvar_name <- paste0("x[", rep(seq_len(NumModules), each = PoolSize), ",", rep(seq_len(PoolSize), NumModules), "]")

  x <- list(
    ItemPool       = ItemPool,
    NumStages      = 1L,
    NumModules     = NumModules,
    NumPathways    = 1L,
    PathwayIndex   = data.frame(pathway_index = 1L, stage1 = 1L),
    decisionvar_name = decisionvar_name
  )
  class(x) <- "mstATA_design"

  # --- Call function --------------------------------------------------------
  levels   <- c("A", "B")
  operator <- ">="
  target_num <- c(1, 2)   # at least 1 A and 2 B in the panel

  con <- panel_itemcat_con(
    x         = x,
    attribute = "content",
    cat_levels    = levels,
    operator  = operator,
    target_num= target_num
  )

  # --- Basic structure checks ----------------------------------------------
  expect_s3_class(con, "mstATA_constraint")
  expect_equal(nrow(con$A_binary), length(levels))      # one row per category
  expect_equal(ncol(con$A_binary), length(decisionvar_name))
  expect_equal(con$operators, rep(check_operator(operator), length(levels)))
  expect_equal(con$d, target_num)

  # --- Column names ---------------------------------------------------------
  expect_equal(colnames(con$A_binary), decisionvar_name)

  # --- Row patterns: each row has (#items in category * NumModules) ones ---
  row_sums <- Matrix::rowSums(con$A_binary)

  # Category "A": 2 items, 2 modules => 4 positions
  # Category "B": 2 items, 2 modules => 4 positions
  expect_equal(as.numeric(row_sums), c(4, 4))

  # --- Specification checks -------------------------------------------------
  expect_equal(con$specification$Type, "Categorical")
  expect_equal(con$specification$`Application Level`, "Panel-level")
  expect_equal(con$specification$`Num of Constraints`, length(levels))
})

test_that("panel_itemcat_con builds panel-level categorical constraints correctly", {
  content_levels<-paste0("content",1:4)
  itempool<-data.frame(item_id = paste0("item",1:100),
                       content = sample(content_levels,100,replace = TRUE))
  which_content1<-which(itempool$content=="content1")
  num_content1<-length(which_content1)
  x<-mst_design(itempool = itempool,design = "1-3",module_length = c(4,7,7,7))
  out<-panel_itemcat_con(x = x,attribute = "content",cat_levels = "content1",operator = ">=",target_num = 5)
  expect_equal(out$d,5)
  expect_equal(out$operators,">=")
  expect_equal(as.vector(apply(out$A_binary,1,sum)),num_content1*4)
  expect_equal(which(as.vector(out$A_binary[1,])==1),c(which_content1,which_content1+100,which_content1+200,which_content1+300))
})
