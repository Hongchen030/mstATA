test_that("stimquant_con errors when x is not mstATA_design", {
  expect_error(
    stimquant_con(
      x = list(),
      attribute = "words",
      min = 100
    ),
    "must be an object of class 'mstATA_design'"
  )
})

test_that("stimquant_con errors when both min and max are NULL", {
  itempool <- data.frame(
    item_id = 1:4,
    stim_id = c("S1", "S1", "S2", "S2"),
    is_pivot = c("P", NA, "P", NA),
    words = c(100, 120, 80, 90),
    stringsAsFactors = FALSE
  )
  stim_map <- create_pivot_stimulus_map(itempool,item_id_col = "item_id", "stim_id", "is_pivot")

  x <- mst_design(itempool, design = "1-2", pathway_length = 3,
                  pivot_stim_map = stim_map)

  expect_error(
    stimquant_con(
      x = x,
      attribute = "words",
      min = NULL,
      max = NULL
    ),
    "At least one of 'min','max' must be provided."
  )
})


test_that("stimquant_con errors when both module and pathway are specified", {
  itempool <- data.frame(
    item_id = 1:4,
    stim_id = c("S1", "S1", "S2", "S2"),
    is_pivot = c("P", NA, "P", NA),
    words = c(100, 120, 80, 90)
  )

  stim_map <- create_pivot_stimulus_map(itempool,item_id_col = "item_id", "stim_id", "is_pivot")

  x <- mst_design(itempool, design = "1-2", pathway_length = 3,
                  pivot_stim_map = stim_map)

  expect_error(
    stimquant_con(
      x = x,
      attribute = "words",
      min = 90,
      which_module = 1,
      which_pathway = 1
    ),
    "Specify either modules or pathways, not both."
  )
})

test_that("stimquant_con errors when min/max are not numeric scalars", {
  itempool <- data.frame(
    item_id = 1:4,
    stim_id = c("S1", "S1", "S2", "S2"),
    is_pivot = c("P", NA, "P", NA),
    words = c(100, 120, 80, 90)
  )

  stim_map <- create_pivot_stimulus_map(itempool,item_id_col = "item_id", "stim_id", "is_pivot")

  x <- mst_design(itempool, design = "1-2", pathway_length = 3,
                  pivot_stim_map = stim_map)

  expect_error(
    stimquant_con(
      x = x,
      attribute = "words",
      min = c(90, 100)
    ),
    "must be a numeric scalar"
  )

  expect_error(
    stimquant_con(
      x = x,
      attribute = "words",
      max = "bad"
    ),
    "must be a numeric scalar"
  )
})

test_that("stimquant_con errors when min > max", {
  itempool <- data.frame(
    item_id = 1:4,
    stim_id = c("S1", "S1", "S2", "S2"),
    is_pivot = c("P", NA, "P", NA),
    words = c(100, 120, 80, 90)
  )

  stim_map <- create_pivot_stimulus_map(itempool,item_id_col = "item_id", "stim_id", "is_pivot")

  x <- mst_design(itempool, design = "1-2", pathway_length = 3,
                  pivot_stim_map = stim_map)

  expect_error(
    stimquant_con(
      x = x,
      attribute = "words",
      min = 120,
      max = 100
    ),
    "'min' cannot be greater than 'max'"
  )
})

test_that("stimquant_con builds correct lower-bound constraints (sign check)", {
  # S1 has 100 words, S2 has 80 words
  itempool <- data.frame(
    item_id = 1:4,
    stim_id = c("S1", "S1", "S2", "S2"),
    is_pivot = c("P", NA, "P", NA),
    words = c(100, 120, 80, 90)
  )

  stim_map <- create_pivot_stimulus_map(itempool,item_id_col = "item_id", "stim_id", "is_pivot")

  x <- mst_design(itempool, design = "1-2", pathway_length = 3,
                  pivot_stim_map = stim_map)

  res <- stimquant_con(
    x = x,
    attribute = "words",
    min = 90
  )
  NumModules<-3
  NumStimulus<-2
  expect_equal(apply(res$A_binary,1,sum),rep(90,NumModules*NumStimulus))
  expect_equal(res$d,rep(c(100,80),NumModules))
  expect_equal(res$operators,rep("<=",NumModules*NumStimulus))
})

test_that("stimquant_con builds correct upper-bound constraints (sign check)", {
  # S1 has 100 words, S2 has 80 words
  itempool <- data.frame(
    item_id = 1:4,
    stim_id = c("S1", "S1", "S2", "S2"),
    is_pivot = c("P", NA, "P", NA),
    words = c(100, 120, 80, 90)
  )

  stim_map <- create_pivot_stimulus_map(itempool,item_id_col = "item_id", "stim_id", "is_pivot")

  x <- mst_design(itempool, design = "1-2", pathway_length = 3,
                  pivot_stim_map = stim_map)

  res <- stimquant_con(
    x = x,
    attribute = "words",
    max = 90   # S2 satisfies (80 <= 90), S1 does not (100 > 90)
  )

  NumModules<-3
  NumStimulus<-2
  expect_equal(apply(res$A_binary,1,sum),rep(c(100,80),NumModules))
  expect_equal(res$d,rep(90,NumStimulus*NumModules))
  expect_equal(res$operators,rep("<=",NumModules*NumStimulus))

})

