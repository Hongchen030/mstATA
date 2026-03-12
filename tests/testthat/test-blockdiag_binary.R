test_that("blockdiag_binary expands binary constraints correctly", {
  A <- Matrix::Matrix(
    c(1, 0,
      0, 1),
    nrow = 2,
    sparse = TRUE
  )

  A_mp <- blockdiag_binary(A, 2)

  expect_equal(dim(A_mp), c(4, 4))

  expect_equal(
    as.matrix(A_mp),
    rbind(
      c(1, 0, 0, 0),
      c(0, 1, 0, 0),
      c(0, 0, 1, 0),
      c(0, 0, 0, 1)
    )
  )
})

test_that("blockdiag_binary returns input for single panel", {
  A <- Matrix::Matrix(1, nrow = 1, ncol = 1, sparse = TRUE)
  expect_identical(blockdiag_binary(A, 1), A)
})
