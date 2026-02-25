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

test_that("test_rdp_con builds routing information balance constraints", {

  design <- make_design_with_iif(theta = c(-0.5, 0.5))

  con <- test_rdp_con(
    x = design,
    rdp = c(-0.5, 0.5),
    which_stage = 1,
    info_tol = 0.4
  )

  expect_s3_class(con, "mstATA_constraint")

  ## structural checks
  expect_true(is.matrix(con$A_binary) || inherits(con$A_binary, "Matrix"))
  expect_equal(nrow(con$A_binary), 4)  # 2 RDPs × 2 inequalities
  expect_equal(length(con$operators), 4)
  expect_equal(length(con$d), 4)

  ## specification checks
  expect_true("specification" %in% names(con))
  expect_equal(con$specification$`Application Level`, "Module-level")
  expect_equal(con$specification$Type, "Quantitative")
})


test_that("test_rdp_con errors when rdp length is incorrect", {

  design <- make_design_with_iif(theta = 0)

  expect_error(
    test_rdp_con(
      x = design,
      rdp = c(0),        # too short
      which_stage = 1
    ),
    "require.*routing decision points"
  )
})


test_that("test_rdp_con errors when routing is defined after final stage", {

  design <- make_design_with_iif(theta = 0)

  expect_error(
    test_rdp_con(
      x = design,
      rdp = 0,
      which_stage = design$NumStages
    ),
    "final stage"
  )
})


test_that("test_rdp_con errors when required iif(theta) is missing", {

  design <- make_test_mstATA_BU()

  expect_error(
    test_rdp_con(
      x = design,
      rdp = c(-0.5,0.5),
      which_stage = 1
    ),
    "iif\\(theta"
  )
})


test_that("test_rdp_con produces symmetric difference constraints", {

  design <- make_design_with_iif(theta = c(-0.5,0.5))

  con <- test_rdp_con(
    x = design,
    rdp = c(-0.5,0.5),
    which_stage = 1,
    info_tol = 0.5
  )

  A <- as.matrix(con$A_binary)

  ## first and second row should be identical (>= and <=)
  expect_equal(A[1, ], A[2, ])

  ## nonzero entries exist
  expect_true(any(A != 0))
})
