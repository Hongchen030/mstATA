test_that("range constraint: single min -> test_itemquant_range_con", {
  x <- make_test_mstATA_BU()
  res <- test_itemquant_range_con(
    x = x,
    attribute = "time",
    min = 200
  )

  expect_s3_class(res, "mstATA_constraint")
  expect_equal(res$operators, rep(">=",x$NumModules))
  expect_equal(res$d, rep(200,x$NumModules))
})

test_that("range constraint: single max -> test_itemquant_range_con", {
  x <- make_test_mstATA_BU()
  res <- test_itemquant_range_con(
    x = x,
    attribute = "time",
    max = 400
  )

  expect_equal(res$operators, rep("<=",x$NumModules))
  expect_equal(res$d, rep(400,x$NumModules))
})

test_that("range constraint: min+max -> two constraints", {
  x <- make_test_mstATA_BU()
  res <- test_itemquant_range_con(
    x = x,
    attribute = "time",
    min = 200, max = 400
  )

  expect_equal(dim(res$specification),c(2,6))
})

test_that("range constraint: target±deviation -> equivalent min/max", {
  x <- make_test_mstATA_BU()
  res <- test_itemquant_range_con(
    x = x,
    attribute = "time",
    target = 300,
    deviation = 10
  )

  expect_equal(res$d, c(rep(290,x$NumModules),rep(310,x$NumModules)))
})

test_that("range constraint: invalid max < min throws error", {
  x <- make_test_mstATA_BU()

  expect_error(
    test_itemquant_range_con(
      x = x,
      attribute = "time",
      min = 400, max = 200
    ),
    "Invalid bounds"
  )
})

test_that("range constraint: providing only one of target/deviation errors", {
  x <- make_test_mstATA_BU()

  expect_error(
    test_itemquant_range_con(
      x = x,
      attribute = "time",
      target = 300
    ),
    "You must either provide 'min' and 'max', or 'target' and 'deviation'."
  )
})
