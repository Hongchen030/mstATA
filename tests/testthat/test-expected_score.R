test_that("expected_score works for a single ICC matrix (on-grid)", {

  ## 2 dichotomous items
  ## item 1: P(1)=0.3, item 2: P(1)=0.7
  icc_mat <- matrix(
    c(0.7, 0.3,
      0.3, 0.7),
    nrow = 2,
    byrow = TRUE
  )

  ## expected score = 0.3 + 0.7 = 1
  expect_equal(
    expected_score(icc_mat),
    1
  )
})

test_that("expected_score works for polytomous ICC matrix", {

  ## 1 item, categories 0,1,2
  ## P = (0.2, 0.5, 0.3)
  icc_mat <- matrix(c(0.2, 0.5, 0.3), nrow = 1)

  ## expected score = 0*0.2 + 1*0.5 + 2*0.3 = 1.1
  expect_equal(
    expected_score(icc_mat),
    1.1
  )
})

test_that("expected_score interpolates correctly for ICC list (off-grid theta)", {

  ## Build simple ICC list with known expected scores
  ## Two dichotomous items

  icc_list <- list(
    "theta=-1" = matrix(
      c(0.9, 0.1,
        0.8, 0.2),
      nrow = 2, byrow = TRUE
    ),
    "theta=1" = matrix(
      c(0.2, 0.8,
        0.1, 0.9),
      nrow = 2, byrow = TRUE
    )
  )

  ## Expected scores:
  ## theta = -1: 0.1 + 0.2 = 0.3
  ## theta =  1: 0.8 + 0.9 = 1.7

  ## At theta = 0 → linear interpolation → (0.3 + 1.7) / 2 = 1.0
  expect_equal(
    expected_score(icc_list, target_theta = 0),
    1.0
  )
})

test_that("expected_score allows extrapolation outside theta grid", {

  icc_list <- list(
    "theta=-1" = matrix(c(0.6, 0.4), nrow = 1),
    "theta=1"  = matrix(c(0.2, 0.8), nrow = 1)
  )

  ## expected scores:
  ## -1 → 0.4
  ##  1 → 0.8

  ## rule = 2 → constant extrapolation
  expect_equal(
    expected_score(icc_list, target_theta = -10),
    0.4
  )

  expect_equal(
    expected_score(icc_list, target_theta = 10),
    0.8
  )
})

test_that("expected_score errors when target_theta is missing for ICC list", {

  icc_list <- list(
    "theta=0" = matrix(c(0.5, 0.5), nrow = 1)
  )

  expect_error(
    expected_score(icc_list),
    "target_theta must be provided"
  )
})

test_that("expected_score errors when ICC list is unnamed", {

  icc_list <- list(
    matrix(c(0.5, 0.5), nrow = 1),
    matrix(c(0.4, 0.6), nrow = 1)
  )

  expect_error(
    expected_score(icc_list, target_theta = 0),
    "named list"
  )
})

test_that("expected_score errors for invalid input type", {

  expect_error(
    expected_score(icc = 123),
    "either a numeric matrix or a list"
  )
})

test_that("expected_score handles unsorted theta names correctly", {

  icc_list <- list(
    "theta=1" = matrix(c(0.1, 0.9), nrow = 1),
    "theta=-1" = matrix(c(0.8, 0.2), nrow = 1)
  )

  ## expected scores:
  ## -1 → 0.2
  ##  1 → 0.9
  ## theta = 0 → 0.55

  expect_equal(
    expected_score(icc_list, target_theta = 0),
    0.55
  )
})
