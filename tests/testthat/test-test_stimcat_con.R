test_that("stimuluscategory_constraint errors when x is not mstATA_design", {
  expect_error(
    test_stimcat_con(
      x = list(),
      attribute = "cat",
      cat_levels = c("A","B"),
      operator = ">=",
      target_num = c(2,3),
      which_module = 1
    ),
    "must be an object of class 'mstATA_design'"
  )
})

test_that("stimuluscategory_constraint errors when operator invalid", {
  expect_error(
    test_stimcat_con(
      x = make_test_mstATA_BU(),
      attribute = "stimulus_type",
      cat_levels = c("history","social studies"),
      operator = "??",
      target_num = c(2,3),
      which_module = 1
    ),
    "Invalid operator"
  )
})


test_that("stimuluscategory_constraint builds correct module-level constraint", {
  x <- make_test_mstATA_BU()
  res <- test_stimcat_con(
    x = x,
    attribute = "stimulus_type",
    cat_levels = c("history","social studies"),
    operator = ">=",
    target_num = c(1, 2),
    which_module = 1
  )

  # Check matrix dims: (levels × modules) × total_decisions
  expect_equal(nrow(res$A_binary), 2)
  expect_equal(ncol(res$A_binary), length(x$decisionvar_name))

  expect_equal(res$operators, rep(">=", 2))
  expect_equal(res$d, c(1,2))

  # item pool 5 history, 5 social studies
  ItemPool<-x$ItemPool
  expect_equal(as.vector(which(res$A_binary[1,] == 1)),
               intersect(which(ItemPool$pivot_item=="Y"),which(ItemPool$stimulus_type=="history")))
  expect_equal(apply(res$A_binary,1,sum),c(5,5))
})

test_that("stimuluscategory_constraint builds correct pathway-level constraint", {
  x <- make_test_mstATA_BU()
  # 3-stage MST
  res <- test_stimcat_con(
    x = x,
    attribute = "stimulus_type",
    cat_levels = c("history","social studies"),
    operator = "<=",
    target_num = c(3,4),
    which_pathway = 1
  )

  expect_equal(res$operators, rep("<=", 2))
  expect_equal(res$d, c(3,4))

  expect_equal(apply(res$A_binary,1,sum),c(15,15))
})

test_that("specification table is correct", {
  x <- make_test_mstATA_BU()

  res <- test_stimcat_con(
    x = x,
    attribute = "stimulus_type",
    cat_levels = c("history","social studies"),
    operator = "=",
    target_num = c(2, 3),
    which_module = 1
  )

  spec <- res$specification
  expect_equal(spec$Attribute, "stimulus_type")
  expect_equal(spec$Type, "Categorical")
  expect_equal(spec$Operator, "(exact)")
  expect_equal(spec$`Application Level`, "Module-level")
  expect_equal(spec$`Num of Constraints`, 2)
})
