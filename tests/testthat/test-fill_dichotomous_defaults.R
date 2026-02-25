test_that("fill_dichotomous_defaults fills and validates parameters correctly", {

  ## 2PL
  p <- matrix(c(1.2, -0.5), nrow = 1)
  out <- fill_dichotomous_defaults(p)

  expect_equal(as.vector(out[, "a"]), 1.2)
  expect_equal(as.vector(out[, "b"]), -0.5)
  expect_equal(as.vector(out[, "g"]), 0)
  expect_equal(as.vector(out[, "d"]), 1)

  ## 1PL
  p <- matrix(0.3, nrow = 1)
  out <- fill_dichotomous_defaults(p)

  expect_equal(as.vector(out[, "a"]), 1)
  expect_equal(as.vector(out[, "b"]), 0.3)
  expect_equal(as.vector(out[, "g"]), 0)
  expect_equal(as.vector(out[, "d"]), 1)

  # 3PL
  p <- matrix(c(0.3,0.2,0.1), nrow = 1)
  out <- fill_dichotomous_defaults(p)

  expect_equal(as.vector(out[, "a"]), 0.3)
  expect_equal(as.vector(out[, "b"]), 0.2)
  expect_equal(as.vector(out[, "g"]), 0.1)
  expect_equal(as.vector(out[, "d"]), 1)

  # 4PL
  p <- matrix(c(0.3,0.2,0.1,0.9), nrow = 1)
  out <- fill_dichotomous_defaults(p)

  expect_equal(as.vector(out[, "a"]), 0.3)
  expect_equal(as.vector(out[, "b"]), 0.2)
  expect_equal(as.vector(out[, "g"]), 0.1)
  expect_equal(as.vector(out[, "d"]), 0.9)
})

test_that("check item parameters",{
  expect_error(fill_dichotomous_defaults(matrix(c(-0.3,0.2,0.1,0.9), nrow = 1)),
               "Discrimination parameter 'a' should be positive")
  expect_error(fill_dichotomous_defaults(matrix(c(0.3,NA,0.1,0.9), nrow = 1)),
               "Difficulty parameter 'b' contains NA")
  expect_error(fill_dichotomous_defaults(matrix(c(0.3,0.2,1.1,0.9), nrow = 1)),
               "Guessing parameter 'g' should be between 0 and 1")
  expect_error(fill_dichotomous_defaults(matrix(c(0.3,0.2,0.1,1.9), nrow = 1)),
               "Upper asymptote parameter 'd' should be between 0 and 1")
  expect_error(fill_dichotomous_defaults(matrix(c(0.3,0.2,0.9,0.1), nrow = 1)),
               "Upper asymptote 'd' must be strictly greater than guessing 'g'")
})
