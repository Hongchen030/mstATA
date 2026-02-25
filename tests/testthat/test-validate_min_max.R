test_that("min bound validation works", {

   # Valid min
  expect_silent({
    validate_min_max(min = 2, max = NULL)
  })

})

test_that("max bound validation warns correctly", {
  # Valid max
  expect_silent({
    validate_min_max(min = NULL, max = 2)
  })
})

test_that("min/max consistency is checked", {
  expect_error(
    validate_min_max(min = 4, max = 3),
    "'min' cannot be greater than 'max'"
  )
})
