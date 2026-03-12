test_that("expand_binary_dv_to_panels expands correctly", {
  dv <- c("x[1,1]", "x[1,2]")
  out <- expand_binary_dv_to_panels(dv, 2)

  expect_equal(
    out,
    c("x[1,1,1]", "x[1,2,1]", "x[1,1,2]", "x[1,2,2]")
  )
})

test_that("expand_binary_dv_to_panels handles single panel", {
  dv <- c("x[1,1]", "x[2,3]")
  expect_equal(expand_binary_dv_to_panels(dv, 1), c("x[1,1,1]","x[2,3,1]"))
})
