test_that("add_row_ranges_from_spec computes correct row ranges", {

  spec <- data.frame(
    Requirement = c("A", "B", "C"),
    `Num of Constraints` = c(2L, 3L, 1L),
    stringsAsFactors = FALSE,check.names = FALSE
  )

  base_row <- 0L

  out <- add_row_ranges_from_spec(spec, base_row)

  expect_equal(out$Row_Start, c(1L, 3L, 6L))
  expect_equal(out$Row_End,   c(2L, 5L, 6L))
})


test_that("add_row_ranges_from_spec respects non-zero base_row", {

  spec <- data.frame(
    Requirement = c("A", "B"),
    `Num of Constraints` = c(4L, 2L),
    stringsAsFactors = FALSE,check.names = FALSE
  )

  base_row <- 10L

  out <- add_row_ranges_from_spec(spec, base_row)

  expect_equal(out$Row_Start, c(11L, 15L))
  expect_equal(out$Row_End,   c(14L, 16L))
})


test_that("add_row_ranges_from_spec preserves original columns", {

  spec <- data.frame(
    Requirement = "A",
    Attribute   = "Content",
    `Num of Constraints` = 3L,
    stringsAsFactors = FALSE,check.names = FALSE
  )

  out <- add_row_ranges_from_spec(spec, base_row = 0L)
  expect_equal(out$Row_Start, 1L)
  expect_equal(out$Row_End,   3L)
  expect_true(all(c("Requirement", "Attribute", "Num of Constraints") %in%
                    names(out)))
})


test_that("Row ranges are contiguous and non-overlapping", {

  spec <- data.frame(
    `Num of Constraints` = c(1L, 2L, 4L),check.names = FALSE
  )

  out <- add_row_ranges_from_spec(spec, base_row = 0L)

  expect_equal(out$Row_Start[-1L], out$Row_End[-nrow(out)] + 1L)
})


test_that("add_row_ranges_from_spec works with a single-row spec", {

  spec <- data.frame(
    `Num of Constraints` = 5L,check.names = FALSE
  )

  out <- add_row_ranges_from_spec(spec, base_row = 3L)

  expect_equal(out$Row_Start, 4L)
  expect_equal(out$Row_End,   8L)
})
