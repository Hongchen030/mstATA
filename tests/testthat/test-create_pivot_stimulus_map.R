test_that("create_pivot_stimulus_map works with valid input", {
  # mock ItemPool
  ItemPool <- data.frame(
    item_id = 1:6,
    stimulus_id = c("S1", "S1", "S2", "S2", "S3", "S3"),
    pivot_flag = c(1, NA, 1, NA, 1, NA)
  )
  ItemPool$pivot_flag<-as.character(ItemPool$pivot_flag)
  # call the function
  result <- create_pivot_stimulus_map(
    itempool = ItemPool,
    stimulus = "stimulus_id",
    pivot_item = "pivot_flag"
  )

  expect_type(result, "list")
  expect_named(result, c("pivot_item_id", "stimulus_name","stimulus_members","numItems_stimulus"))
  expect_equal(length(result$pivot_item_id), 3)
  expect_equal(result$stimulus_name, c("S1", "S2", "S3"))
  expect_true(all(result$numItems_stimulus == 2))
  expect_equal(result$pivot_item_id,as.character(c(1,3,5)))
})


test_that("create_pivot_stimulus_map errors when no stimulus column", {
  ItemPool <- data.frame(item_id = 1:3, stim_col = NA_character_, pivot_flag = c(1, 0, 0))
  expect_error(
    create_pivot_stimulus_map(itempool = ItemPool, stimulus = "stim_col",pivot_item =  "pivot_flag"),
    "No stimulus information found in column: stim_col")
})


test_that("create_pivot_stimulus_map errors when no pivot items found", {
  ItemPool <- data.frame(
    item_id = 1:4,
    stimulus_id = c("S1", "S1", "S2", "S2"),
    pivot_flag = NA_character_
  )
  expect_error(
    create_pivot_stimulus_map(ItemPool,item_id_col = "item_id", "stimulus_id", "pivot_flag"),
    "No pivot items found in column: pivot_flag")
})


test_that("create_pivot_stimulus_map errors when pivot count mismatched with stimuli", {
  ItemPool <- data.frame(
    item_id = 1:4,
    stimulus_id = c("S1", "S1", "S2", "S2"),
    pivot_flag = c(1, 1, 1, NA)
  )
  ItemPool$pivot_flag<-as.character(ItemPool$pivot_flag)

  expect_error(
    create_pivot_stimulus_map(ItemPool, item_id_col = "item_id","stimulus_id", "pivot_flag"),
    fixed = TRUE,
    "Invalid: Stimulus 'S1' has 2 pivot items (must have exactly one)."
  )

  ItemPool <- data.frame(
    item_id = 1:4,
    stimulus_id = c("S1", "S1", "S2", "S2"),
    pivot_flag = c(1, NA, NA, NA)
  )
  ItemPool$pivot_flag<-as.character(ItemPool$pivot_flag)

  expect_error(
    create_pivot_stimulus_map(ItemPool,item_id_col = "item_id", "stimulus_id", "pivot_flag"),
    fixed = TRUE,
    "Invalid: Stimulus 'S2' has 0 pivot items (must have exactly one)."
  )
})


test_that("create_pivot_stimulus_map errors when pivot item not tied to a stimulus", {
  ItemPool <- data.frame(
    item_id = 1:3,
    stimulus_id = c("S1", "S1", NA),
    pivot_flag = c(1, NA, 1)
  )
  ItemPool$pivot_flag<-as.character(ItemPool$pivot_flag)

  expect_error(
    create_pivot_stimulus_map(ItemPool,item_id_col = "item_id", "stimulus_id", "pivot_flag"),
    "Each pivot item should be associated"
  )
})
