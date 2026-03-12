test_that("onepanel_spec computes correct row ranges for constraints only", {
  # 1-3-3, c(4, 2, 2, 2, 2, 2, 2)
  x <- make_test_mstATA_BU()

  spec1 <- test_itemcount_con(x = x,stage_length_bound = NULL)
  spec2 <- test_itemcat_con(x = x,attribute = "itemtype",cat_levels = "MC",operator = "=",target_num = 4,which_pathway  = 1)
  ps <- onepanel_spec(
    x = x,
    constraints = list(spec1,spec2)
  )

  expect_s3_class(ps, "mstATA_model")

  spec <- ps$specification

  expect_equal(spec$Row_Start, c(1, 8))
  expect_equal(spec$Row_End,   c(7, 8))
})

test_that("objective-related constraints receive correct row range", {

  data("mini_itempool")
  x <- mst_design(itempool = mini_itempool,
                  design = "1-3-3",module_length = c(4,2,2,2,3,3,3))

  spec1 <- test_itemcount_con(x = x,stage_length_bound = NULL)
  spec2 <- test_itemcat_con(x = x,attribute = "itemtype",cat_levels = "MC",operator = "=",target_num = 4,which_pathway  = 1)
  obj1<-objective_term(x = x,attribute = "iif(theta=-1)",applied_level = "Pathway-level",which_pathway = 1,
                       sense = "max")
  obj2<-objective_term(x = x,attribute = "iif(theta=1)",applied_level = "Pathway-level",which_pathway = 9,
                       sense = "max")
  obj<-maximin_obj(x = x,multiple_terms = list(obj1,obj2))
  ps <- onepanel_spec(
    x = x,
    constraints = list(spec1,spec2),
    objective = obj
  )

  spec <- ps$specification

  expect_equal(nrow(spec), 4)

  ## constraint rows
  expect_equal(spec$Row_Start[1], 1)
  expect_equal(spec$Row_End[1],   7)

  ## objective rows (2 rows after constraints)
  expect_equal(spec$Row_Start[3], 9)
  expect_equal(spec$Row_End[3],   9)
  expect_equal(spec$Row_Start[4], 10)
  expect_equal(spec$Row_End[4],   10)

  expect_equal(spec$Source, c("Constraint","Constraint", "Objective","Objective"))
})

test_that("dvlink_item_stim is auto-added and included in row ranges", {

  data("reading_itempool")
  NumStimuli<-length(unique(reading_itempool$stimulus))
  NumModules<-7
  pivot_stim_map <- create_pivot_stimulus_map(itempool   = reading_itempool,
                                              stimulus   = "stimulus",
                                              pivot_item = "pivot_item")
  x <- mst_design(itempool = reading_itempool,design   = "1-3-3",
                  module_length = c(14,12,12,12,12,12,12),
                  pivot_stim_map = pivot_stim_map)

  con <- stim_itemcount_con(x = x,
                            min = 5,max = 10,which_module = 1)

  ps <- onepanel_spec(
    x = x,
    constraints = list(con)
  )

  spec <- ps$specification

  ## original constraint
  expect_equal(nrow(spec), 1)

  expect_equal(spec$Row_Start[1], 1)
  expect_equal(spec$Row_End[1],(2*NumStimuli))
})


test_that("multi-panel solution-level constraint is rejected", {

  data("mini_itempool")
  x <- mst_design(itempool = mini_itempool,
                  design = "1-3-3",module_length = c(4,2,2,2,3,3,3))

  spec1 <- test_itemcount_con(x = x,stage_length_bound = NULL)
  spec2 <- solution_itemcat_con(x = x,attribute = "itemtype",cat_levels = "MC",operator = "<=",29)

  expect_error(
    onepanel_spec(x = x, constraints = list(spec1,spec2)),
    "multiple panels"
  )
})


