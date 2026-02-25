make_design_with_iif <- function(theta) {
  design <- make_test_mstATA_BU()
  ItemPool<-design$ItemPool
  ItemPool[,paste0("iif(theta=",theta,")")] <- compute_iif(design$ItemPool,
                                                           item_par_cols = list("3PL"=c("discrimination","difficulty","guessing"),
                                                                                "GRM"=c("alphaj","betaj1","betaj2")),
                                                           theta = theta,model_col = "model")
  design$ItemPool<-ItemPool
  design
}

test_that("mst_structure_con combines item count and routing constraints", {

  design <-make_design_with_iif(theta = c(-0.5, 0.5,-1,1))
  ## 1-3-3 BU, 7 modules
  con <- mst_structure_con(
    x = design,
    info_tol = 0.4
  )

  ## combined object: 7+4*2 = 15
  expect_s3_class(con, "mstATA_constraint")
  ## should contain two components
  expect_equal(nrow(con$A_binary),15)
})

test_that("mst_structure_con works with item count constraints only", {

  design <- mst_design(itempool = test_itempool,
                       design = "1-3-3",
                       module_length = c(4,2,2,2,2,2,2),
                       pivot_stim_map = pivot_stim_map,
                       enemyitem_set = enemyitem_set,
                       enemystim_set = enemystim_set)

  con <- mst_structure_con(
    x = design,
    stage_length_bound = NULL
  )

  expect_s3_class(con, "mstATA_constraint")
  expect_equal(nrow(con$A_binary),7)
})



test_that("mst_structure_con propagates errors from test_rdp_con", {

  design <- make_test_mstATA_BU()

  expect_error(
    mst_structure_con(
      x = design
    ),
    "iif\\(theta"
  )
})

test_that("mst_structure_con propagates errors from test_itemcount_con", {

  design <- make_test_mstATA_BU()

  expect_error(
    mst_structure_con(
      x = design,
      stage_length_bound = -1
    )
  )
})

