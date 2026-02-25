
test_that("compute_icc_by_path row-binds module ICCs correctly", {

  ## fake ICC structure
  icc_by_mod <- list(
    "1" = list(
      "theta=0" = matrix(1:4, nrow = 2),
      "theta=1" = matrix(5:8, nrow = 2)
    ),
    "2" = list(
      "theta=0" = matrix(9:12, nrow = 2),
      "theta=1" = matrix(13:16, nrow = 2)
    )
  )

  res <- compute_icc_by_path(icc_by_mod, c(1, 2))

  expect_type(res, "list")
  expect_equal(names(res), c("theta=0", "theta=1"))
  expect_equal(nrow(res[[1]]), 4)  # 2 + 2
  expect_equal(ncol(res[[1]]), 2)
})

