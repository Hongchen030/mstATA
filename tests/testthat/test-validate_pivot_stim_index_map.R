test_that("validate_pivot_stim_index_map fails when indices are outside itempool", {

  itempool <- data.frame(item_id = 1:4)

  bad_map <- list(
    pivot_item_id = 5L,  # invalid
    stimulus_name = "S1",
    stimulus_members = list(c(1L, 5L)),
    numItems_stimulus = 2L
  )

  expect_error(
    validate_pivot_stim_index_map(bad_map, itempool),
    "pivot_item_id.*outside"
  )
})

test_that("validate_pivot_stim_index_map succeeds for a valid mapping", {

  itempool <- data.frame(
    item_id = paste0("I", 1:6),
    stringsAsFactors = FALSE
  )

  pivot_stim_map <- list(
    pivot_item_id = as.integer(c(2, 5)),
    stimulus_name = c("S1", "S2"),
    stimulus_members = list(
      c(1L, 2L, 3L),
      c(4L, 5L, 6L)
    ),
    numItems_stimulus = c(3L, 3L)
  )

  expect_true(
    validate_pivot_stim_index_map(pivot_stim_map, itempool)
  )
})
