test_that("expand_spec_by_panel replicates specification by panel", {

  spec_single <- data.frame(
    Requirement = c("Same-length modules", "Item overlap across modules"),
    Type = c("Categorical", "Logical"),
    `Application Level` = c("Module-level", "Panel-level"),
    `Num of Constraints` = c(2L, 3L),
    Row_Start = c(1L, 3L),
    Row_End   = c(2L, 5L)
  )

  rows_per_panel <- 5L
  panels <- 1:3

  out <- expand_spec_by_panel(
    spec_single = spec_single,
    rows_per_panel = rows_per_panel,
    panels = panels
  )

  ## ---- total rows ----
  expect_equal(nrow(out), nrow(spec_single) * length(panels))

  ## ---- panel labels ----
  expect_equal(out$Panel, rep(panels, each = nrow(spec_single)))

  ## ---- row shifting ----
  expected_start <- c(
    1L, 3L,               # panel 1
    1L + 5L, 3L + 5L,     # panel 2
    1L + 10L, 3L + 10L    # panel 3
  )

  expected_end <- c(
    2L, 5L,
    2L + 5L, 5L + 5L,
    2L + 10L, 5L + 10L
  )

  expect_equal(out$Row_Start, expected_start)
  expect_equal(out$Row_End, expected_end)
})


test_that("expand_spec_by_panel preserves original specification fields", {

  spec_single <- data.frame(
    Requirement = "Pathway item count",
    Attribute = "Item_id",
    Type = "Categorical",
    `Application Level` = "Pathway-level",
    Operator = "exact",
    `Num of Constraints` = 4L,
    Row_Start = 1L,
    Row_End = 4L,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  out <- expand_spec_by_panel(
    spec_single = spec_single,
    rows_per_panel = 4L,
    panels = 1:2
  )

  ## ---- unchanged columns ----
  expect_equal(out$Requirement, rep("Pathway item count", 2))
  expect_equal(out$Attribute, rep("Item_id", 2))
  expect_equal(out$Operator, rep("exact", 2))
  expect_equal(out$`Num of Constraints`, rep(4L, 2))
})


test_that("expand_spec_by_panel works with a single panel", {

  spec_single <- data.frame(
    Requirement = "TEI item count",
    Type = "Categorical",
    `Application Level` = "Pathway-level",
    `Num of Constraints` = 3L,
    Row_Start = 1L,
    Row_End = 3L,
    stringsAsFactors = FALSE
  )

  out <- expand_spec_by_panel(
    spec_single = spec_single,
    rows_per_panel = 3L,
    panels = 1L
  )

  expect_equal(nrow(out), 1L)
  expect_equal(out$Panel, 1L)
  expect_equal(out$Row_Start, 1L)
  expect_equal(out$Row_End, 3L)
})
