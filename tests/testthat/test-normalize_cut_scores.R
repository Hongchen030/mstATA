test_that("normalize_cut_scores returns score cuts unchanged", {

  cuts <- c(5, 10)

  out <- normalize_cut_scores(
    cut_scale = "score",
    cuts = cuts
  )

  expect_equal(out, cuts)
  expect_type(out, "double")
})

test_that("normalize_cut_scores converts theta cuts to score scale", {

  ## simple ICC list: 1 dichotomous item
  icc_list <- list(
    "theta=-1" = matrix(c(0.8, 0.2), nrow = 1),
    "theta=1"  = matrix(c(0.2, 0.8), nrow = 1)
  )

  ## expected scores:
  ## -1 -> 0.2
  ##  1 -> 0.8
  ## theta = 0 -> 0.5

  cuts_theta <- c(0)

  out <- normalize_cut_scores(
    cut_scale = "theta",
    cuts = cuts_theta,
    icc_list = icc_list
  )

  expect_equal(out, 0.5)
  expect_true(out %% 1 != 0)  # fractional cut allowed
})

test_that("normalize_cut_scores preserves ordering of cuts", {

  icc_list <- list(
    "theta=-2" = matrix(c(0.9, 0.1), nrow = 1),
    "theta=0"  = matrix(c(0.5, 0.5), nrow = 1),
    "theta=2"  = matrix(c(0.1, 0.9), nrow = 1)
  )

  cuts_theta <- c(-1, 1)

  out <- normalize_cut_scores(
    "theta",
    cuts = cuts_theta,
    icc_list = icc_list
  )

  expect_true(is.numeric(out))
  expect_true(out[1] < out[2])
})

test_that("normalize_cut_scores errors for non-increasing cuts", {

  expect_error(
    normalize_cut_scores("score", cuts = c(5, 5)),
    "strictly increasing"
  )

  expect_error(
    normalize_cut_scores("score", cuts = c(10, 5)),
    "strictly increasing"
  )
})

test_that("normalize_cut_scores errors when icc_list missing for theta cuts", {

  expect_error(
    normalize_cut_scores("theta", cuts = c(0)),
    "icc_list"
  )
})

test_that("normalize_cut_scores errors for invalid cut_scale", {

  expect_error(
    normalize_cut_scores(cut_scale ="ability", cuts = c(5, 10)),
    "should be one of"
  )
})

test_that("normalize_cut_scores allows extrapolation via expected_score", {

  icc_list <- list(
    "theta=-1" = matrix(c(0.7, 0.3), nrow = 1),
    "theta=1"  = matrix(c(0.2, 0.8), nrow = 1)
  )

  ## rule = 2 => constant extrapolation
  out_low <- normalize_cut_scores(
    "theta",
    cuts = c(-10),
    icc_list = icc_list
  )

  out_high <- normalize_cut_scores(
    "theta",
    cuts = c(10),
    icc_list = icc_list
  )

  expect_equal(out_low, 0.3)
  expect_equal(out_high, 0.8)
})
