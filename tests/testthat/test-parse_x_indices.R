test_that("parse_x_indices parses valid x[module,item,panel] names", {
  varname_df <- data.frame(
    varname = c("x[1,2,3]", "x[10,20,30]"),
    stringsAsFactors = FALSE
  )

  out <- parse_x_indices(varname_df)

  expect_true(is.data.frame(out))
  expect_true(all(c("module_id", "item_id", "panel_id") %in% names(out)))

  expect_equal(out$module_id, c(1L, 10L))
  expect_equal(out$item_id,   c(2L, 20L))
  expect_equal(out$panel_id,  c(3L, 30L))
})

test_that("parse_x_indices drops non-matching varnames", {
  varname_df <- data.frame(
    varname = c("x[1,2,3]", "s[2]", "x[1,2]", "x[1,2,3,4]", "x[a,2,3]"),
    stringsAsFactors = FALSE
  )

  out <- parse_x_indices(varname_df)

  # Only the strict match with digits and no spaces should remain
  expect_equal(out$varname, "x[1,2,3]")
  expect_equal(out$module_id, 1L)
  expect_equal(out$item_id,   2L)
  expect_equal(out$panel_id,  3L)
})

test_that("parse_x_indices drops non-matching varnames", {
  varname_df <- data.frame(
    varname = c("x[1,2,3]", "s[2]", "s[3]", "s[4]", "s[5]"),
    stringsAsFactors = FALSE
  )

  out <- parse_x_indices(varname_df)

  # Only the strict match with digits and no spaces should remain
  expect_equal(out$varname, "x[1,2,3]")
  expect_equal(out$module_id, 1L)
  expect_equal(out$item_id,   2L)
  expect_equal(out$panel_id,  3L)
})


test_that("parse_x_indices preserves other columns for kept rows", {
  varname_df <- data.frame(
    varname = c("x[1,2,3]", "bad", "x[4,5,6]"),
    coef = c(0.1, 0.2, 0.3),
    stringsAsFactors = FALSE
  )

  out <- parse_x_indices(varname_df)

  expect_equal(out$coef, c(0.1, 0.3))
  expect_equal(out$module_id, c(1L, 4L))
  expect_equal(out$item_id,   c(2L, 5L))
  expect_equal(out$panel_id,  c(3L, 6L))
})

test_that("parse_x_indices returns 0-row data frame when no names match", {
  varname_df <- data.frame(
    varname = c("y[1,2,3]", "x[1,2]", "x[1,2,3,4]", "x[1,2, ]"),
    stringsAsFactors = FALSE
  )

  out <- parse_x_indices(varname_df)

  expect_true(is.data.frame(out))
  expect_equal(nrow(out), 0L)
  expect_true(all(c("module_id", "item_id", "panel_id") %in% names(out)))
})

