test_that("compute_iif works for dichotomous models", {

  models <- c("1PL", "RASCH", "2PL", "3PL", "4PL")

  item_par_cols <- list(
    "1PL"   = c("a", "b"),
    "RASCH" = c("b"),
    "2PL"   = c("a", "b"),
    "3PL"   = c("a", "b", "c"),
    "4PL"   = c("a", "b", "c", "d")
  )

  for (m in models) {

    items <- data.frame(
      item_id = paste0("i", 1:3),
      model   = m,
      a = c(1.0, 1.2, 0.8),
      b = c(-1, 0, 1),
      c = c(0, 0, 0.2),
      d = c(1, 1, 1)
    )

    iif <- compute_iif(
      items = items,
      item_par_cols = item_par_cols,
      theta = c(-1, 0, 1),
      model_col = "model"
    )

    expect_equal(dim(iif), c(3, 3))
    expect_equal(colnames(iif), c("theta=-1", "theta=0", "theta=1"))
    expect_true(all(is.finite(iif)))
    expect_true(all(iif >= 0))
  }
})

test_that("compute_iif works for polytomous models", {

  items <- data.frame(
    item_id = paste0("p", 1:5),
    model   = c("GRM", "PCM", "GPCM", "RSM", "NRM"),
    p1 = c(1.0, 0.5, 1.2, 0.8, 1.0),
    p2 = c(-1, NA, 0.4, 0.3, 0.0),
    p3 = c(1, NA, NA, NA, 1),
    p4 = c(NA, NA, NA, NA, 0)
  )

  item_par_cols <- list(
    GRM  = c("p1", "p2", "p3"),
    PCM  = c("p1"),
    GPCM = c("p1", "p2"),
    RSM  = c("p1", "p2"),
    NRM  = c("p1", "p2", "p3", "p4")
  )

  iif <- compute_iif(
    items = items,
    item_par_cols = item_par_cols,
    theta = c(-1, 0, 1),
    model_col = "model"
  )

  expect_equal(dim(iif), c(5, 3))
  expect_true(all(is.finite(iif)))
  expect_true(all(iif >= 0))
})

test_that("compute_iif works for mixed item formats", {

  items <- data.frame(
    item_id = paste0("m", 1:4),
    model   = c("3PL", "PCM", "GRM", "NRM"),
    p1 = c(1.2, 0.5, 1.0, 1.0),
    p2 = c(0.0, NA, -1.0, 0.0),
    p3 = c(0.2, NA, 1.0, 1.0),
    p4 = c(NA, NA, NA, 0.0)
  )

  item_par_cols <- list(
    "3PL" = c("p1", "p2", "p3"),
    "PCM" = c("p1"),
    "GRM" = c("p1", "p2", "p3"),
    "NRM" = c("p1", "p2", "p3", "p4")
  )

  iif <- compute_iif(
    items = items,
    item_par_cols = item_par_cols,
    theta = c(-1, 0),
    model_col = "model"
  )

  expect_equal(dim(iif), c(4, 2))
  expect_true(all(is.finite(iif)))
  expect_true(all(iif >= 0))
})

test_that("compute_iif errors on unsupported model", {

  items <- data.frame(
    item_id = "x1",
    model   = "BADMODEL",
    p1 = 1
  )
  item_par_cols <- list(
    "3PL" = c("p1", "p2", "p3"),
    "PCM" = c("p1"),
    "GRM" = c("p1", "p2", "p3"),
    "NRM" = c("p1", "p2", "p3", "p4")
  )
  expect_error(
    compute_iif(
      items = items,
      item_par_cols = item_par_cols,
      theta = 0,
      model_col = "model"
    ),
    "Unsupported model"
  )
})




