test_that("assembled_panel returns correct panel structure", {

  ## ---- mock mstATA_model ----
  x <- list(
    ItemPool = data.frame(
      item_id = c("I1", "I2", "I3"),
      content = c("MC", "MC", "TEI"),
      stringsAsFactors = FALSE
    ),
    item_id_col = "item_id",
    Item_IDs = c(I1 = 1L, I2 = 2L, I3 = 3L),
    NumStages = 2,
    NumPathways = 2,
    PathwayIndex = data.frame(
      stage1 = c(1, 1),
      stage2 = c(2, 3),
      pathway_index = c(1, 2),
      pathway_label = c("E-H", "E-M"),
      stringsAsFactors = FALSE
    ),
    ModuleIndex = data.frame(
      module_index = c(1, 2, 3),
      module_label = c("S1E", "S2H", "S2M"),
      stringsAsFactors = FALSE
    )
  )
  class(x) <- "mstATA_design"

  ## ---- mock solver result ----
  result <- list(
    model = list(
      vtype = c("B", "B", "C"),
      varname = c("x[1,1,1]", "x[2,2,1]", "z[1]"),
      stringsAsFactors = FALSE
    ),
    solution = list(
      best_solution = c(1, 1, 0)
    )
  )

  ## ---- run ----
  out <- assembled_panel(x, result)

  ## ---- expectations ----
  expect_type(out, "list")
  expect_named(out, "Panel_1")

  panel1 <- out$Panel_1

  expect_true(all(c("ItemsInModules", "ItemsInPathways") %in% names(panel1)))

  expect_true(is.data.frame(panel1$ItemsInModules))
  expect_true(is.data.frame(panel1$ItemsInPathways))

  expect_true(all(panel1$ItemsInModules$item_name %in% c("I1", "I2")))
})


test_that("assembled_panel errors on invalid inputs", {

  expect_error(
    assembled_panel(x = list(), result = list()),
    "mstATA_design"
  )

  x <- make_test_mstATA_BU()

  expect_error(
    assembled_panel(x, result = list()),
    "solve_model"
  )
})

x<-mst_design(itempool = mini_itempool,item_id_col = "item_id",design = "1-3",module_length = c(3,2,2,2))
con1<-mst_structure_con(x = x,stage_length_bound = NULL)
obj_term<-objective_term(x = x,attribute = "difficulty",applied_level = "Module-level",which_module = 1,goal = 0,sense = "min")
obj<-single_obj(x = x,single_term = obj_term)
model<-onepanel_spec(x = x,constraints = list(con1),objective = obj)
result<-solve_model(model_spec = model,solver = "HiGHS",check_feasibility = FALSE)


test_that("assembled_panel returns valid mstATA_panel object", {

  # assume x and result come from a small synthetic design
  panel <- assembled_panel(x, result)

  expect_s3_class(panel, "mstATA_panel")
  expect_type(panel, "list")

  # correct panel naming
  expect_true(all(grepl("^Panel_", names(panel))))

  for (p in panel) {

    expect_named(p, c("ItemsInModules", "ItemsInPathways"))

    expect_true(is.data.frame(p$ItemsInModules))
    expect_true(is.data.frame(p$ItemsInPathways))

    # required columns
    expect_true(all(c("item_name","module_id") %in%
                      names(p$ItemsInModules)))

    expect_true(all(c("item_name","pathway_id") %in%
                      names(p$ItemsInPathways)))

    # no duplicated item-module within panel
    expect_false(any(duplicated(
      p$ItemsInModules[, c("item_name","module_id")]
    )))
  }
})

test_that("ItemsInPathways respects routing structure", {
  # assume x and result come from a small synthetic design
  panel <- assembled_panel(x, result)

  PathwayIndex <- x$PathwayIndex
  NumStages <- x$NumStages

  for (p in panel) {

    for (path_id in unique(p$ItemsInPathways$pathway_id)) {

      mods_expected <- as.integer(
        as.vector(as.matrix(
          PathwayIndex[PathwayIndex$pathway_index == path_id,
                       1:NumStages]
        ))
      )

      mods_found <- unique(
        p$ItemsInPathways$module_id[
          p$ItemsInPathways$pathway_id == path_id
        ]
      )

      expect_true(all(mods_found %in% mods_expected))
    }
  }
})

test_that("ItemsInModules and ItemsInPathways agree", {

  panel <- assembled_panel(x, result)

  for (p in panel) {

    expect_true(
      all(p$ItemsInPathways$item_name %in%
            p$ItemsInModules$item_name)
    )
  }
})


