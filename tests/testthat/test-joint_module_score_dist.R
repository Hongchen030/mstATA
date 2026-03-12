test_that("joint_module_score_dist returns correct dimensions", {

  prev_scores <- 0:2
  cdist <- matrix(
    c(0.3, 0.3,
      0.4, 0.4,
      0.3, 0.3),
    nrow = 3,
    dimnames = list(NULL, c("t1", "t2"))
  )

  icc <- matrix(
    c(0.5, 0.5,
      0.6, 0.4),
    nrow = 2,
    byrow = TRUE
  )
  colnames(icc) <- c("0", "1")

  icc_by_next <- list(t1 = icc, t2 = icc)

  out <- joint_module_score_dist(
    cdist_by_prev = cdist,
    prev_scores = prev_scores,
    icc_by_next_mod = icc_by_next,
    possible_joint_score = 1:3
  )

  expect_true(is.matrix(out))
  expect_equal(nrow(out), 3)
  expect_equal(ncol(out), 2)
  expect_equal(colnames(out), c("t1", "t2"))
})


test_that("supports sparse possible_joint_score", {

  prev_scores <- 0:2
  cdist <- matrix(
    c(0.2, 0.2,
      0.5, 0.5,
      0.3, 0.3),
    nrow = 3,ncol = 2,
    dimnames = list(NULL, c("t1","t2"))
  )

  icc <- matrix(
    c(0.4, 0.6),
    nrow = 1
  )
  colnames(icc) <- c("0", "1")

  icc_by_next <- list(t1 = icc, t2 = icc)

  out <- joint_module_score_dist(
    cdist,
    prev_scores,
    icc_by_next,
    possible_joint_score = c(0, 2)
  )

  expect_equal(rownames(out), c("0", "2"))
})


test_that("column sums equal routing probability", {

  prev_scores <- 0:1
  cdist <- matrix(
    c(0.6, 0.4),
    nrow = 2,
    dimnames = list(NULL, "t")
  )

  icc <- matrix(
    c(0.5, 0.5),
    nrow = 1
  )
  colnames(icc) <- c("0", "1")

  icc_by_next <- list(t = icc)

  out <- joint_module_score_dist(
    cdist,
    prev_scores,
    icc_by_next,
    possible_joint_score = 0:2
  )

  expect_equal(sum(out[, "t"]), sum(cdist[, "t"]))
})


test_that("errors on theta mismatch", {

  prev_scores <- 0
  cdist <- matrix(1, nrow = 1, dimnames = list(NULL, "t1"))

  icc <- matrix(1, nrow = 1)
  colnames(icc) <- "0"

  icc_by_next <- list(t2 = icc)

  expect_error(
    joint_module_score_dist(
      cdist,
      prev_scores,
      icc_by_next,
      possible_joint_score = 0
    ),
    "Theta grid mismatch"
  )
})

