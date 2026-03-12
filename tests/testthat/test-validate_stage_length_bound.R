test_that("validate_stage_length_bounds() fills defaults when NULL", {
  x <- list(
    NumStages = 2,
    ModuleIndex = data.frame(stage = c(1,2,2,2), module_id = 1:4)
  )
  class(x)<-"mstATA_design"
  result <- validate_stage_length_bounds(x)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 4)
  expect_true(all(c("stage", "module_id", "min", "max") %in% names(result)))
  expect_equal(result$min, rep(1L, 4))
  expect_true(all(is.na(result$max)))
})

test_that("validate_stage_length_bounds() stops for wrong number of columns", {
  x <- list(NumStages = 2, ModuleIndex = data.frame(stage = 1:2))
  class(x)<-"mstATA_design"
  wrong <- data.frame(stage = 1:2, min = 1:2)
  expect_error(validate_stage_length_bounds(x, wrong),
               "'stage_length_bound' must be a data.frame with 3 columns: stage,min,max.")
})

test_that("validate_stage_length_bounds() stops wrong stage_length_bound dataframe", {
  x <- list(NumStages = 2, ModuleIndex = data.frame(stage = 1:2))
  class(x)<-"mstATA_design"

  wrong_1 <- data.frame(mst_stage = 1:2, min = 1:2,max = NA)
  expect_error(validate_stage_length_bounds(x, wrong_1),
               "Columns must be named 'stage', 'min', 'max'.")

  wrong_2<- data.frame(stage = 1:3,min = NA, max = NA)
  expect_error(validate_stage_length_bounds(x,wrong_2),
               "Stage indices in 'stage_length_bound' must be between 1 and NumStages.")

  wrong_3<-data.frame(stage = 1:2,min=-1,max=10)
  expect_error(validate_stage_length_bounds(x,wrong_3),
               "'min' must be non-negative for all stages.")

  wrong_4<-data.frame(stage = 1:2,min = 1,max=-1)
  expect_error(validate_stage_length_bounds(x,wrong_4),
               "'max' must be non-negative for all stages.")

  wrong_5<-data.frame(stage = 1:2,min=c(5,3),max=c(2,3))
  expect_error(validate_stage_length_bounds(x,wrong_5),
               "'min' cannot exceed 'max' for a stage.")
})


test_that("validate_stage_length_bounds() accepts valid input with proper columns", {
  x <- list(
    NumStages = 3,
    ModuleIndex = data.frame(stage = c(1,2,2,2,3,3,3), module_id = 1:7)
  )
  class(x)<-"mstATA_design"

  stage_length_bound <- data.frame(
    stage = 1:3,
    min = c(1L, 2L, 3L),
    max = c(5L, 6L, 7L)
  )

  result <- validate_stage_length_bounds(x, stage_length_bound)

  expect_equal(result$min, c(1L, 2L,2L,2L,3L,3L,3L))
  expect_equal(result$max, c(5L, 6L,6L,6L,7L,7L,7L))
})



test_that("validate_stage_length_bounds() stops for wrong column names", {
  x <- list(NumStages = 2, ModuleIndex = data.frame(stage = 1:2))
  class(x)<-"mstATA_design"

  wrong <- data.frame(a = 1:2, b = 1:2, c = 1:2)
  expect_error(validate_stage_length_bounds(x, wrong),
               "Columns must be named 'stage', 'min', 'max'")
})

test_that("validate_stage_length_bounds() stops when stage index out of range", {
  x <- list(NumStages = 2, ModuleIndex = data.frame(stage = 1:2))
  class(x)<-"mstATA_design"

  bounds <- data.frame(stage = 3, min = 1, max = 2)
  expect_error(validate_stage_length_bounds(x, bounds),
               "Stage indices in 'stage_length_bound' must be between 1 and NumStages.")
})

test_that("validate_stage_length_bounds() stops for negative min or max", {
  x <- list(NumStages = 2, ModuleIndex = data.frame(stage = 1:2))
  class(x)<-"mstATA_design"

  bad_min <- data.frame(stage = 1:2, min = c(-1, 1), max = c(2, 2))
  bad_max <- data.frame(stage = 1:2, min = c(1, 1), max = c(-2, 2))
  expect_error(validate_stage_length_bounds(x, bad_min), "'min' must be non-negative for all stages.")
  expect_error(validate_stage_length_bounds(x, bad_max), "'max' must be non-negative for all stages.")
})

test_that("validate_stage_length_bounds() stops when min exceeds max", {
  x <- list(NumStages = 2, ModuleIndex = data.frame(stage = 1:2))
  class(x)<-"mstATA_design"

  bad <- data.frame(stage = 1:2, min = c(5, 1), max = c(2, 3))
  expect_error(validate_stage_length_bounds(x, bad),
               "'min' cannot exceed 'max' for a stage.")
})

test_that("validate_stage_length_bounds() fills NA mins with 1L", {
  x <- list(
    NumStages = 2,
    ModuleIndex = data.frame(stage = 1:2, module_id = 1:2)
  )
  class(x)<-"mstATA_design"

  bounds <- data.frame(stage = 1:2, min = c(NA, 3), max = c(5, 6))

  result <- validate_stage_length_bounds(x, bounds)
  expect_equal(result$min, c(1L, 3L))
  expect_equal(result$max, c(5, 6))
})
