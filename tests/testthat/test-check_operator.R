test_that("check_operator accepts valid operators", {
  expect_equal(check_operator(c("<=","=")), c("<=","="))
  expect_equal(check_operator("="), "=")
  expect_equal(check_operator(">="), ">=")
})

test_that("check_operator rejects invalid operators", {
  expect_error(check_operator("!="), "Invalid operator")
  expect_error(check_operator("<>"), "Invalid operator")
  expect_error(check_operator("less"), "Invalid operator")
})
