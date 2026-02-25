test_that("multipanel_spec validates inputs", {
  expect_error(multipanel_spec(1, NULL, 2))
})

test_that("multipanel_spec rejects non-model panel_spec", {
  x <- make_test_mstATA_BU()
  expect_error(multipanel_spec(x, list(), 2))
})

test_that("multipanel_spec enforces decision variable consistency", {
  x <- mst_design(mini_itempool,design = "1-3-3",module_length = c(4,5,5,5,3,3,3))
  spec2<-itemcat_con(x,item_ids = c(1,4),select = TRUE,which_module = 1)
  spec3<-panel_itemreuse_con(x,overlap = FALSE)
  spec6<-test_itemcount_con(x)
  spec7<-test_itemquant_con(x,attribute = "iif(theta=-1)",operator = ">=",
                             target_value = 10,which_pathway = 1)
  obj1 <- objective_term(x,"iif(theta=-1)",NULL,"Pathway-level",which_pathway=1)
  obj2 <- objective_term(x,"iif(theta=0)", NULL,"Pathway-level",which_pathway=2)
  obj3 <- objective_term(x,"iif(theta=1)", NULL,"Pathway-level",which_pathway=3)
  obj<-capped_maximin_obj(x,multiple_terms = list(obj1,obj2,obj3))
  panel_model<-onepanel_spec(x = x,constraints = list(spec2,spec3,spec6,spec7),
                          objective = obj)
  panel_model$varname <- paste0("bad", seq_along(panel_model$varname))

  expect_error(multipanel_spec(x, panel_model, 2))
})

fake_mstATA <- function(PoolSize = 5, NumModules = 3) {

  ItemPool <- data.frame(
    item_id = seq_len(PoolSize),
    stringsAsFactors = FALSE
  )

  decisionvar_name <- paste0(
    "x[",
    rep(seq_len(NumModules), each = PoolSize),
    ",",
    rep(seq_len(PoolSize), times = NumModules),
    "]"
  )

  structure(
    list(
      ItemPool = ItemPool,
      NumModules = NumModules,
      decisionvar_name = decisionvar_name,
      item_id_col = "item_id"
    ),
    class = "mstATA_design"
  )
}

fake_panel_model <- function(x) {

  dv_bin <- sub("\\]$", paste0(",", 1, "]"), x$decisionvar_name)
  n_bin <- length(dv_bin)

  A_binary <- Matrix::Matrix(
    data = sample(c(0, 1), size = n_bin * 2, replace = TRUE),
    nrow = 2,
    ncol = n_bin,
    sparse = TRUE
  )

  colnames(A_binary) <- dv_bin

  specification <- data.frame(
    Source = "test",
    Requirement = "fake constraint",
    Attribute = NA_character_,
    Type = "Logical",
    `Application Level` = "Module-level",
    Operator = "<=",
    `Num of Constraints` = nrow(A_binary),
    Row_Start = 1L,
    Row_End = nrow(A_binary),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  structure(
    list(
      name = paste0("fake_constraint_", seq_len(nrow(A_binary))),
      specification = specification,
      A_binary = A_binary,
      A_real = NULL,
      C_binary = rep(0, n_bin),
      C_real = numeric(0),
      operators = rep("<=", nrow(A_binary)),
      d = rep(1, nrow(A_binary)),
      varname = dv_bin,
      vtype = rep("B", n_bin),
      sense = "min",
      lb = NULL,
      ub = NULL
    ),
    class = "mstATA_model"
  )
}

fake_solution_constraint <- function(x) {

  PoolSize <- nrow(x$ItemPool)

  A_binary <- Matrix::Matrix(
    data = 1,
    nrow = 1,
    ncol = PoolSize,
    sparse = TRUE
  )

  colnames(A_binary) <- paste0("s[", seq_len(PoolSize), "]")

  structure(
    list(
      name = "fake_solution_constraint",
      specification = data.frame(
        Requirement = "fake solution constraint",
        Attribute = NA_character_,
        Type = "Logical",
        `Application Level` = "Solution-level",
        Operator = "<=",
        `Num of Constraints` = 1,
        stringsAsFactors = FALSE,
        check.names = FALSE
      ),
      A_binary = A_binary,
      operators = "<=",
      d = PoolSize
    ),
    class = "mstATA_constraint"
  )
}




test_that("multipanel_spec expands binary variables correctly", {
  x <- fake_mstATA()
  panel_model <- fake_panel_model(x)

  model <- multipanel_spec(x, panel_model, num_panels = 3)

  expect_equal(
    sum(model$vtype == "B"),
    length(x$decisionvar_name) * 3
  )
})

test_that("multipanel_spec expands constraint rows correctly", {
  x <- fake_mstATA()
  panel_model <- fake_panel_model(x)

  model <- multipanel_spec(x, panel_model, num_panels = 4)

  expect_equal(
    nrow(model$A_binary),
    nrow(panel_model$A_binary) * 4
  )
})

test_that("solution-level constraints add s variables", {
  x <- fake_mstATA()
  panel_model <- fake_panel_model(x)
  sol_con <- fake_solution_constraint(x)

  model <- multipanel_spec(
    x, panel_model, num_panels = 2,
    solution_con = list(sol_con)
  )

  expect_true(any(grepl("^s\\[", model$varname)))
  PoolSize<-5
  NumModules<-3
  num_panels<-2
  expect_equal(model$lb, rep(0,(PoolSize*NumModules*num_panels+PoolSize)))
  expect_equal(model$ub, rep(1,(PoolSize*NumModules*num_panels+PoolSize)))
})


test_that("multipanel_spec expands a single-panel model to parallel panels", {

  x <- mst_design(itempool = mini_itempool,design = "1-2",module_length = c(10,5,5))

  panel_model <- onepanel_spec(x,constraints = list(test_itemcount_con(x)))

  mp <- multipanel_spec(
    x = x,
    panel_model = panel_model,
    num_panels = 3
  )

  expect_s3_class(mp, "mstATA_model")

  ## binary decision variables expanded
  expect_equal(
    sum(mp$vtype == "B"),
    length(x$decisionvar_name) * 3
  )

  ## no solution variables when no reuse / solution constraints
  expect_false(any(grepl("^s\\[", mp$varname)))

})


test_that("gating constraints are added when reuse bounds are specified", {

  x <- mst_design(itempool = mini_itempool,design = "1-2",module_length = c(10,5,5))

  panel_model <- onepanel_spec(x,constraints = list(test_itemcount_con(x)))

  mp <- multipanel_spec(
    x = x,
    panel_model = panel_model,
    num_panels = 2,
    global_max_use = 1
  )

  PoolSize <- nrow(x$ItemPool)

  ## s[i] introduced
  expect_true(any(grepl("^s\\[", mp$varname)))
  expect_equal(
    sum(grepl("^s\\[", mp$varname)),
    PoolSize
  )

  ## 2 * PoolSize gating rows added
  spec <- mp$specification
  gate_rows <- spec$Requirement == "Item exposure control across panels"
  expect_equal(sum(spec$`Num of Constraints`[gate_rows]), 2 * PoolSize)
})

test_that("multipanel_spec rejects invalid solution constraints", {

  x <- mst_design(itempool = mini_itempool,design = "1-2",module_length = c(10,5,5))

  panel_model <- onepanel_spec(x,constraints = list(test_itemcount_con(x)))

  bad_con <- test_itemcount_con(x)

  expect_error(
    multipanel_spec(
      x = x,
      panel_model = panel_model,
      num_panels = 2,
      solution_con = list(bad_con)
    ),
    "Solution-level constraints"
  )

})


test_that("item-specific reuse bounds override global bounds", {

  x <- mst_design(itempool = mini_itempool,design = "1-2",module_length = c(10,5,5))

  panel_model <- onepanel_spec(x,constraints = list(test_itemcount_con(x)))

  item_max <- data.frame(
    item_id = x$ItemPool$item_id[1],
    max = 0
  )

  mp <- multipanel_spec(
    x = x,
    panel_model = panel_model,
    num_panels = 2,
    global_max_use = 2,
    item_max_use = item_max
  )

  ## solution indicator exists
  expect_true(any(grepl("^s\\[", mp$varname)))

  ## constraint rows exist for item-specific override
  which_row<-which(mp$name=="Max exposure for item 1")
  expect_equal(mp$d[which_row],0)
})

test_that("binary objective coefficients are replicated across panels", {

  x <- mst_design(itempool = mini_itempool,design = "1-2",module_length = c(10,5,5))
  obj <- single_obj(x = x,single_term = objective_term(x,"iif(theta=0)",applied_level = "Module-level",which_module = 1,
                                                       sense = "max"))

  panel_model <- onepanel_spec(
    x,
    constraints = list(test_itemcount_con(x)),
    objective = obj)

  mp <- multipanel_spec(
    x = x,
    panel_model = panel_model,
    num_panels = 3
  )

  expect_equal(mp$C_binary,
               rep(panel_model$C_binary, 3))
})



