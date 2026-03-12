test_that("mst_design returns an mstATA_design object", {

  x <- mst_design(
    itempool = test_itempool,
    design = "1-3",
    pathway_length=5
  )

  expect_s3_class(x, "mstATA_design")
})

test_that("mst_design has no enemy item constraints when enemyitem_set is NULL", {

  x <- mst_design(
    itempool = test_itempool,
    design = "1-3",
    pathway_length = 8,
    enemyitem_set = NULL
  )

  expect_null(x$enemyitem_set)
})

test_that("mst_design() constucts an mstATA_design object for MST 1-3-3", {

  PoolSize<-nrow(test_itempool)
  out <- mst_design(itempool = test_itempool,
                    module_length = c(6, 4, 4, 4, 4, 4, 4),
                    design = "1-3-3")

  expect_true(inherits(out, "mstATA_design"))
  expect_equal(out$NumStages, 3)
  expect_equal(out$NumModules, 7)
  expect_equal(out$NumPathways,9)
  expect_equal(out$ModuleIndex$stage,c(1,2,2,2,3,3,3))
  expect_equal(out$ModuleIndex$module,c(1,1,2,3,1,2,3))
  expect_equal(out$ModuleIndex$module_index,1:7)


  NumModules<-out$NumModules
  expect_equal(out$decisionvar_name,
               paste0("x[", rep(seq_len(NumModules), each = PoolSize), ",", rep(seq_len(PoolSize), NumModules), "]"))
  expect_equal(unique(out$decisionvar_type),"B")
})

test_that("mst_design() fails if both ModuleLength and PathwayLength are inconsistent", {
  expect_error(
    mst_design(
      itempool = test_itempool,
      design = "1-3-3",
      module_length = c(6, 4, 4, 4, 4, 4, 4),
      pathway_length = 13
    ),
    "The provided `pathway_length` is not equal to the length calculated by the sum of stagelength.", fixed = TRUE
  )
})

test_that("mst_design() works with ExcludedPathways", {
  excluded <- c("1-1-3", "1-3-1")

  out <- mst_design(itempool = test_itempool, design = "1-3-3",
                    module_length = c(6, 4, 4, 4, 4, 4, 4),
                    exclude_pathways = excluded)

  expect_true(inherits(out, "mstATA_design"))
  expect_equal(out$NumStages, 3)
  expect_equal(out$NumModules, 7)
  expect_equal(nrow(out$PathwayIndex), 7)  # 9 total - 2 excluded
  expect_true(all(out$PathwayIndex$pathway %in% setdiff(
    c("1-1-1", "1-1-2", "1-1-3", "1-2-1", "1-2-2", "1-2-3", "1-3-1", "1-3-2", "1-3-3"),
    excluded
  )))
})


test_that("mst_design() works with item_module_eligibility list", {
  ItemPool<-data.frame(item_id=paste0("item",1:20),
                       difficulty=seq(-3,3,length.out=20))
  item_module_eligibility<-list(`2`=1:7,`3`=8:13,`4`=14:20,
                                `5`=1:7,`6`=8:13,`7`=14:20)
  PoolSize<-nrow(ItemPool)
  out <- mst_design(itempool = ItemPool,
                    design = "1-3-3",
                    pathway_length = 12,
                    item_module_eligibility = item_module_eligibility)

  expect_true(inherits(out, "mstATA_design"))

  NumModules<-out$NumModules
  expect_equal(out$decisionvar_name,
              c(paste0("x[", rep(1, PoolSize), ",", seq_len(PoolSize), "]"),
                paste0("x[", rep(2,length(1:7)), ",", 1:7, "]"),
                paste0("x[", rep(3,length(8:13)), ",", 8:13, "]"),
                paste0("x[", rep(4,length(14:20)), ",", 14:20, "]"),
                paste0("x[", rep(5,length(1:7)), ",", 1:7, "]"),
                paste0("x[", rep(6,length(8:13)), ",", 8:13, "]"),
                paste0("x[", rep(7,length(14:20)), ",", 14:20, "]")))
  expect_equal(unique(out$decisionvar_type),"B")
  expect_equal(length(out$decisionvar_type),(20+(7+6+7)*2))
})

test_that("mst_design derives ItemIndex for enemyitem_set", {
  itempool <- data.frame(
    item_id = c("A", "B", "C", "D"),
    stringsAsFactors = FALSE
  )
  enemyitem_set <- list(
    ExclusionPair = matrix(
      c("A", "B", "B", "C"),
      ncol = 2,
      byrow = TRUE
    ),
    EnemySet = list(
      c("A", "B"),
      c("B", "C")
    )
  )

  x <- mst_design(
    itempool = itempool,
    design = "1-3",
    pathway_length = 8,
    enemyitem_set = enemyitem_set
  )

  expect_true("ItemIndex" %in% names(x$enemyitem_set))
  expect_equal(x$enemyitem_set$ItemIndex[[1]], c(1L, 2L))
  expect_equal(x$enemyitem_set$ItemIndex[[2]], c(2L, 3L))
})

