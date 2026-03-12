test_that("parse_objective_string detects relative and absolute objectives", {

  rel <- parse_objective_string("Relative Objective: Maximize the sum of diff values in Module 1")
  abs <- parse_objective_string("Absolute Objective: Minimize the number of items from MC category in a Panel")

  expect_equal(rel$objective_type, "Relative objective")
  expect_equal(abs$objective_type, "Absolute objective")
  expect_equal(rel$attribute,"diff")
  expect_equal(rel$attribute_type,"Quantitative")
  expect_equal(rel$application_level,"Module-level")
  expect_equal(abs$attribute,"MC")
  expect_equal(abs$attribute_type,"Categorical")
  expect_equal(abs$application_level,"Panel-level")
  expect_equal(abs$operator,"(min)")
})

test_that("parse_objective_string extracts quantitative attribute", {

  obj <- parse_objective_string(
    "Relative Objective: Maximize the sum of difficulty values in Pathway 3"
  )

  expect_equal(obj$attribute, "difficulty")
  expect_equal(obj$attribute_type, "Quantitative")
})

test_that("parse_objective_string extracts categorical attribute", {
  obj_string<-paste0("Absolute Objective: Minimize the absolute deviation between",
                     " the number of items from category 'MC'"," and the target value of ",10," in a panel")
  obj <- parse_objective_string(obj_string = obj_string)

  expect_equal(obj$attribute, "MC")
  expect_equal(obj$attribute_type, "Categorical")
})

test_that("parse_objective_string detects level", {

  mod <- parse_objective_string("Maximize the sum of foo values in Module 1")
  path <- parse_objective_string("Minimize the sum of bar values in Pathway 2")
  panel <- parse_objective_string("Minimize the number of items from category 'MC' in a panel")

  expect_equal(mod$application_level,"Module-level")
  expect_equal(path$application_level,"Pathway-level")
  expect_equal(panel$application_level,"Panel-level")
  expect_equal(panel$attribute,"MC")
})


test_that("parse_objective_string detects operator max/min", {

  max_obj <- parse_objective_string("Maximize the sum of difficulty values")
  min_obj <- parse_objective_string("Minimize the number of items from category 'MC'")

  expect_equal(max_obj$operator, "(max)")
  expect_equal(min_obj$operator, "(min)")
})

test_that("parse_objective_string returns NA for unrecognized strings", {

  obj <- parse_objective_string("This is not a valid objective phrase")

  expect_true(is.na(obj$objective_type))
  expect_true(is.na(obj$attribute))
  expect_true(is.na(obj$attribute_type))
  expect_true(is.na(obj$application_level))
  expect_true(is.na(obj$operator))
})

