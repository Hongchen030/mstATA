test_that("stimcat_con errors for non-mstATA_design input", {
  expect_error(
    stimcat_con(
      x = list(),
      stim_ids = 1
    ),
    "must be an object of class 'mstATA_design'"
  )
})

# -------------------------------------------------------------------
# Build a small test itempool with two stimuli
# -------------------------------------------------------------------
test_itempool <- data.frame(
  item_id = 1:8,
  stim_id = c("S1","S1","S1","S2","S2","S2", NA, NA),
  is_pivot = c("P", NA, NA, "P", NA, NA, NA, NA),
  stringsAsFactors = FALSE
)

test_stim_map <- create_pivot_stimulus_map(
  itempool = test_itempool,
  stimulus = "stim_id",
  pivot_item = "is_pivot"
)

x <- mst_design(
  itempool = test_itempool,
  design = "1-2",
  pathway_length = 3,
  pivot_stim_map = test_stim_map
)

# -------------------------------------------------------------------
# Stimulus ID validation
# -------------------------------------------------------------------
test_that("stimcat_con errors for invalid character stim_ids", {
  expect_error(
    stimcat_con(
      x = x,
      stim_ids = "S999"
    ),
    "not in the item pool"
  )
})

test_that("stimcat_con errors for invalid numeric stim_ids", {
  expect_error(
    stimcat_con(
      x = x,
      stim_ids = 99
    ),
    "valid stimulus indices"
  )
})

test_that("stimcat_con errors for bad stim_id type", {
  expect_error(
    stimcat_con(
      x = x,
      stim_ids = list("S1")
    ),
    "character stimulus names or numeric indices"
  )
})

# -------------------------------------------------------------------
# Correct resolution of stim_ids → pivot_items
# -------------------------------------------------------------------
test_that("Character stim_ids correctly map to pivot items", {
  out <- stimcat_con(
    x = x,
    stim_ids = "S1"
  )

  # S1 pivot should be item 1
  expect_true(any(grepl("stimulus S1", out$name)))
})

test_that("Numeric stim_ids correctly map to pivot items", {
  out <- stimcat_con(
    x = x,
    stim_ids = 2  # second stimulus → S2
  )

  expect_true(any(grepl("stimulus S2", out$name)))
})

# -------------------------------------------------------------------
# Duplicate stim_ids handling
# -------------------------------------------------------------------
test_that("Duplicate stim_ids are ignored with a warning", {
  expect_warning(
    stimcat_con(
      x = x,
      stim_ids = c("S1","S1")
    ),
    "duplicates removed"
  )
})

# -------------------------------------------------------------------
# Correct naming replacement: item → stimulus
# -------------------------------------------------------------------
test_that("Constraint names replace item_ID with stimulus_name", {
  out <- stimcat_con(
    x = x,
    stim_ids = "S1"
  )

  # Should contain "stimulus S1" instead of "item 1"
  expect_true(any(grepl("stimulus S1", out$name)))
  expect_false(any(grepl("item 1", out$name)))
})

# -------------------------------------------------------------------
# Constraint generation: module scope
# -------------------------------------------------------------------
test_that("Module-scope stimcat_con returns valid constraint", {

  out <- stimcat_con(
    x = x,
    stim_ids = "S1",
    which_module = 1
  )

  expect_true(is.matrix(out$A_binary) || inherits(out$A_binary, "dgCMatrix"))
  expect_equal(out$specification$`Application Level`[1], "Module-level")
  expect_true(all(out$operators == "="))
})

# -------------------------------------------------------------------
# Constraint generation: pathway scope
# -------------------------------------------------------------------
test_that("Pathway-scope stimcat_con works with naming", {

  out <- stimcat_con(
    x = x,
    stim_ids = "S1",
    which_pathway = 1
  )

  expect_equal(out$specification$`Application Level`[1], "Pathway-level")
  expect_true(any(grepl("stimulus S1", out$name)))
})

# -------------------------------------------------------------------
# Constraint generation: panel scope (default)
# -------------------------------------------------------------------
test_that("Panel-scope stimcat_con works by default", {

  out <- stimcat_con(
    x = x,
    stim_ids = "S1"
  )

  expect_equal(out$specification$`Application Level`[1], "Panel-level")
  expect_true(any(grepl("stimulus S1", out$name)))
})

# -------------------------------------------------------------------
# select = FALSE (omit)
# -------------------------------------------------------------------
test_that("stimcat_con works with select = FALSE", {

  out <- stimcat_con(
    x = x,
    stim_ids = "S2",
    select = FALSE
  )

  expect_true(all(out$d == 0))
  expect_true(any(grepl("Not select stimulus S2", out$specification$Requirement)))
})

