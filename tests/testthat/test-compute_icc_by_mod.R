test_that("compute_icc_by_mod returns correct nested structure", {

  items <- data.frame(
    item_id   = paste0("i", 1:4),
    module_id = c(1, 1, 2, 2),
    model     = "2PL",
    a         = c(1, 1, 1, 1),
    b         = c(0, 0, 0, 0)
  )

  item_par_cols <- list("2PL" = c("a", "b"))
  theta <- c(-1, 0, 1)

  out <- compute_icc_by_mod(
    items_in_modules = items,
    item_par_cols = item_par_cols,
    model_col = "model",
    theta = theta
  )

  ## top-level structure
  expect_type(out, "list")
  expect_length(out, 2)

  ## module names
  expect_equal(names(out), c("1", "2"))

  ## second level: theta lists
  mod1 <- out[[1]]
  expect_type(mod1, "list")
  expect_length(mod1, length(theta))

  ## theta names
  expect_true(all(grepl("theta", names(mod1))))

  ## matrices: items × categories
  mat <- mod1[[1]]
  expect_true(is.matrix(mat))
  expect_equal(nrow(mat), 2)  # 2 items in module 1
  expect_equal(ncol(mat), 2)  # dichotomous → 2 categories
})

test_that("compute_icc_by_mod errors if module_id is missing", {

  items <- data.frame(
    item_id = "i1",
    model   = "2PL",
    a = 1,
    b = 0
  )

  expect_error(
    compute_icc_by_mod(
      items_in_modules = items,
      item_par_cols = list("2PL" = c("a", "b")),
      model_col = "model"
    ),
    "module_id"
  )
})

test_that("compute_icc_by_mod errors on invalid theta", {

  items <- data.frame(
    item_id   = "i1",
    module_id = 1,
    model     = "2PL",
    a = 1,
    b = 0
  )

  expect_error(
    compute_icc_by_mod(
      items_in_modules = items,
      item_par_cols = list("2PL" = c("a", "b")),
      model_col = "model",
      theta = c(0, NA)
    ),
    "theta"
  )
})

