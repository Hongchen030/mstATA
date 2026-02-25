test_that("validate_pivot_stim_map errors when required fields are missing", {

  stim_map_missing <- list(
    pivot_item_id = 1:3,
    stimulus_name = c("S1","S2","S3")
    # missing stimulus_members, numItems_stimulus
  )

  expect_error(
    validate_pivot_stim_map(stim_map_missing),
    "must contain fields"
  )
})

test_that("validate_pivot_stim_map errors when lengths do not match", {

  stim_map_badlen <- list(
    pivot_item_id     = c(3, 10),
    stimulus_name     = c("S1"),              # mismatch
    stimulus_members  = list(1:4, 10:12),
    numItems_stimulus = c(4, 3)
  )

  expect_error(
    validate_pivot_stim_map(stim_map_badlen),
    "lengths.*must match"
  )
})

test_that("validate_pivot_stim_map errors when pivot_item_id has duplicates", {

  stim_map_duppivot <- list(
    pivot_item_id     = c(3, 3),
    stimulus_name     = c("S1", "S2"),
    stimulus_members  = list(1:4, 5:6),
    numItems_stimulus = c(4, 2)
  )

  expect_error(
    validate_pivot_stim_map(stim_map_duppivot),
    "Invalid pivot_stim_map: pivot_item_id contains duplicates."
  )
})

test_that("validate_pivot_stim_map errors when stimulus_name has duplicates", {

  stim_map_dupstim <- list(
    pivot_item_id     = c(3, 10),
    stimulus_name     = c("S1", "S1"),
    stimulus_members  = list(1:4, 5:6),
    numItems_stimulus = c(4, 2)
  )

  expect_error(
    validate_pivot_stim_map(stim_map_dupstim),
    "stimulus_name contains duplicates"
  )
})

test_that("validate_pivot_stim_map errors when membership count mismatches", {

  stim_map_badcount <- list(
    pivot_item_id     = c(3, 10),
    stimulus_name     = c("S1","S2"),
    stimulus_members  = list(1:4, 5:6),
    numItems_stimulus = c(4, 5)   # wrong count for S2
  )

  expect_error(
    validate_pivot_stim_map(stim_map_badcount),
    "Invalid pivot_stim_map: numItems_stimulus does not match stimulus_members."
  )
})

test_that("validate_pivot_stim_map errors when pivot item is not in its stimulus membership", {

  stim_map_pivot_missing <- list(
    pivot_item_id     = c(3, 10),
    stimulus_name     = c("S1", "S2"),
    stimulus_members  = list(1:4, 5:7),   # pivot=10 missing
    numItems_stimulus = c(4, 3)
  )

  expect_error(
    validate_pivot_stim_map(stim_map_pivot_missing),
    "pivot item.*is not included"
  )
})

test_that("validate_pivot_stim_map errors when stimulus memberships overlap", {

  stim_map_overlap <- list(
    pivot_item_id     = c(3, 10),
    stimulus_name     = c("S1", "S2"),
    stimulus_members  = list(1:4, c(4,5,6,7,8,9,10)),  # item 4 overlaps
    numItems_stimulus = c(4, 7)
  )

  expect_error(
    validate_pivot_stim_map(stim_map_overlap),
    "Invalid pivot_stim_map: items cannot belong to multiple stimuli."
  )
})


test_that("validate_pivot_stim_map returns TRUE for a valid map", {

  itempool<-data.frame(item_id = 1:12)
  stim_map_valid <- list(
    pivot_item_id     = c(3, 10),
    stimulus_name     = c("S1", "S2"),
    stimulus_members  = list(1:4, 10:12),
    numItems_stimulus = c(4, 3)
  )

  expect_true(validate_pivot_stim_map(stim_map_valid,itempool,"item_id"))
})

