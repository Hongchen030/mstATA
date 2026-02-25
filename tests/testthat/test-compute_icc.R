test_that("compute_icc works for dichotomous models (1PL–4PL)", {

  models <- c("1PL", "RASCH", "2PL", "3PL", "4PL")

  for (m in models) {

    items <- data.frame(
      item_id = paste0("i", 1:3),
      model   = m,
      a = c(1, 1.2, 0.8),
      b = c(-1, 0, 1),
      c = c(0, 0, 0.2),
      d = c(1, 1, 1)
    )

    item_par_cols <- list(
      "1PL"   = c("a", "b"),
      "RASCH" = c("b"),
      "2PL"   = c("a", "b"),
      "3PL"   = c("a", "b", "c"),
      "4PL"   = c("a", "b", "c", "d")
    )

    icc <- compute_icc(
      items = items,
      item_par_cols = item_par_cols,
      theta = c(-1, 0, 1),
      model_col = "model"
    )


    expect_length(icc, 3)

    for (mat in icc) {
      expect_equal(dim(mat), c(3, 2))
      expect_true(all(abs(rowSums(mat) - 1) < 1e-8))
      expect_equal(colnames(mat), c("cat0", "cat1"))
    }
  }
})

test_that("compute_icc works for polytomous models", {

  items <- data.frame(
    item_id = paste0("p", 1:5),
    model   = c("GRM", "PCM", "GPCM", "RSM", "NRM"),
    p1 = c(1, 0.5, 1, 0.2, 1),
    p2 = c(-1, NA, 0.4, 0.3, 0),
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

  icc <- compute_icc(
    items = items,
    item_par_cols = item_par_cols,
    theta = 0,
    model_col = "model"
  )

  mat <- icc[[1]]

  expect_equal(nrow(mat), 5)
  expect_true(ncol(mat) >= 2)

  # Each item sums to 1 across its valid categories
  for (i in seq_len(5)) {
    expect_true(abs(sum(mat[i, ]) - 1) < 1e-8)
  }
})

test_that("compute_icc works for mixed item formats", {

  items <- data.frame(
    item_id = paste0("m", 1:4),
    model   = c("3PL", "PCM", "GRM", "NRM"),
    p1 = c(1, 0.5, 1, 1),
    p2 = c(0, NA, -1, 0),
    p3 = c(0.2, NA, 1, 1),
    p4 = c(NA, NA, NA, 0)
  )

  item_par_cols <- list(
    "3PL" = c("p1", "p2", "p3"),
    "PCM" = c("p1"),
    "GRM" = c("p1", "p2", "p3"),
    "NRM" = c("p1", "p2", "p3", "p4")
  )

  icc <- compute_icc(
    items = items,
    item_par_cols = item_par_cols,
    theta = c(-1, 0),
    model_col = "model"
  )

  expect_length(icc, 2)

  for (mat in icc) {
    expect_equal(nrow(mat), 4)
    expect_true(all(abs(rowSums(mat) - 1) < 1e-8))
    expect_true(all(grepl("^cat", colnames(mat))))
  }
})

test_that("compute_icc errors when nrCat is inconsistent for dichotomous models", {

  items <- data.frame(
    item_id = "i1",
    model = "2PL",
    a = 1,
    b = 0,
    nrCat = 3
  )

  item_par_cols <- list(
    "2PL" = c("a", "b")
  )

  expect_error(
    compute_icc(
      items = items,
      item_par_cols = item_par_cols,
      theta = seq(-2, 2, 1),
      model_col = "model",
      nrCat_col = "nrCat"
    ),
    "Inconsistent nrCat"
  )
})

test_that("compute_icc errors when nrCat is inconsistent for GRM", {

  items <- data.frame(
    item_id = "i1",
    model = "GRM",
    a = 1,
    b1 = -1,
    b2 = 1,
    nrCat = 2
  )

  item_par_cols <- list(
    "GRM" = c("a", "b1", "b2")
  )

  expect_error(
    compute_icc(
      items = items,
      item_par_cols = item_par_cols,
      theta = seq(-2, 2, 1),
      model_col = "model",
      nrCat_col = "nrCat"
    ),
    "Inconsistent nrCat"
  )
})

test_that("compute_icc errors when nrCat is inconsistent for PCM", {

  items <- data.frame(
    item_id = "i1",
    model = "PCM",
    b1 = -1,
    b2 = 1,
    nrCat = 4
  )

  item_par_cols <- list(
    "PCM" = c("b1", "b2")
  )

  expect_error(
    compute_icc(
      items = items,
      item_par_cols = item_par_cols,
      theta = seq(-2, 2, 1),
      model_col = "model",
      nrCat_col = "nrCat"
    ),
    "Inconsistent nrCat"
  )
})

test_that("compute_icc errors when nrCat is inconsistent for NRM", {

  items <- data.frame(
    item_id = "i1",
    model = "NRM",
    a1 = 1, b1 = -1,
    a2 = 1, b2 = 1,
    nrCat = 4
  )

  item_par_cols <- list(
    "NRM" = c("a1", "b1", "a2", "b2")
  )

  expect_error(
    compute_icc(
      items = items,
      item_par_cols = item_par_cols,
      theta = seq(-2, 2, 1),
      model_col = "model",
      nrCat_col = "nrCat"
    ),
    "Inconsistent nrCat"
  )
})

test_that("compute_icc fails if any item has inconsistent nrCat", {

  items <- data.frame(
    item_id = c("i1", "i2"),
    model = c("2PL", "PCM"),
    a = c(1, NA),
    b = c(0, NA),
    b1 = c(NA, -1),
    b2 = c(NA, 1),
    nrCat = c(2, 4)
  )

  item_par_cols <- list(
    "2PL" = c("a", "b"),
    "PCM" = c("b1", "b2")
  )

  expect_error(
    compute_icc(
      items = items,
      item_par_cols = item_par_cols,
      theta = seq(-2, 2, 1),
      model_col = "model",
      nrCat_col = "nrCat"
    ),
    "Inconsistent nrCat"
  )
})
