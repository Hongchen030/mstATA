test_that("check_nonneg_integer accepts valid nonnegative integer matrices", {
  A <- matrix(c(0, 1, 2, 3), nrow = 2)
  expect_silent(check_nonneg_integer(A))

  B <- matrix(c(0L, 5L, 10L), nrow = 1)
  expect_silent(check_nonneg_integer(B))
})

test_that("check_nonneg_integer errors on non-integer values", {
  A <- matrix(c(0, 1.2, 2), nrow = 1)

  expect_error(
    check_nonneg_integer(A),
    "integer"
  )
})

test_that("check_nonneg_integer errors on negative values", {
  A <- matrix(c(0, -1, 2), nrow = 1)

  expect_error(
    check_nonneg_integer(A),
    "nonnegative"
  )
})

test_that("check_nonneg_integer errors on NA, NaN, or Inf", {
  A1 <- matrix(c(0, NA, 1), nrow = 1)
  A2 <- matrix(c(0, NaN, 1), nrow = 1)
  A3 <- matrix(c(0, Inf, 1), nrow = 1)

  expect_error(check_nonneg_integer(A1), "NA|NaN|infinite")
  expect_error(check_nonneg_integer(A2), "NA|NaN|infinite")
  expect_error(check_nonneg_integer(A3), "NA|NaN|infinite")
})


test_that("check_nonneg_integer errors on non-numeric matrices", {
  A <- matrix(c("a", "b"), nrow = 1)

  expect_error(
    check_nonneg_integer(A),
    "numeric"
  )
})

