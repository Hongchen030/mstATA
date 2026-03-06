




test_that("inv_tcc_from_icc works for dichotomous items (3PL)", {

  set.seed(123)

  items <- data.frame(
    a = runif(10, 0.8, 1.5),
    b = rnorm(10),
    g = rep(0.2, 10),
    model = rep("3PL", 10)
  )

  item_par_cols <- list(
    "3PL" = c("a","b","g")
  )

  res <- inv_tcc_from_icc(
    items = items,
    item_par_cols = item_par_cols,
    model_col = "model"
  )

  expect_s3_class(res, "data.frame")
  expect_true(all(c("sum.score","est.theta") %in% names(res)))

  expect_equal(res$sum.score, 0:10)

  expect_true(all(is.finite(res$est.theta)))

  expect_true(all(diff(res$est.theta) >= 0))

})

test_that("inv_tcc_from_icc works for polytomous items (GRM)", {

  set.seed(123)

  items <- data.frame(
    a = runif(5, 0.8, 1.5),
    b1 = rnorm(5),
    b2 = rnorm(5),
    b3 = rnorm(5),
    model = rep("GRM",5)
  )

  item_par_cols <- list(
    "GRM" = c("a","b1","b2","b3")
  )

  res <- inv_tcc_from_icc(
    items = items,
    item_par_cols = item_par_cols,
    model_col = "model"
  )

  expect_s3_class(res, "data.frame")

  expect_true(all(diff(res$est.theta) >= 0))

  expect_true(min(res$est.theta) >= -5)
  expect_true(max(res$est.theta) <= 5)

})

test_that("inv_tcc_from_icc works for mixed item formats", {

  set.seed(123)

  items <- data.frame(
    a = c(runif(5,0.8,1.5), runif(3,0.6,1.2)),
    b = c(rnorm(5), rep(NA,3)),
    g = c(rep(0.2,5), rep(NA,3)),
    b1 = c(rep(NA,5), rnorm(3)),
    b2 = c(rep(NA,5), rnorm(3)),
    model = c(rep("3PL",5), rep("GRM",3))
  )

  item_par_cols <- list(
    "3PL" = c("a","b","g"),
    "GRM" = c("a","b1","b2")
  )

  res <- inv_tcc_from_icc(
    items = items,
    item_par_cols = item_par_cols,
    model_col = "model"
  )

  expect_s3_class(res,"data.frame")

  expect_true(all(diff(res$est.theta) >= 0))

  expect_true(length(res$sum.score) > 0)

})

test_that("inverse TCC roughly matches expected score", {

  set.seed(123)

  items <- data.frame(
    a = runif(8,0.8,1.5),
    b = rnorm(8),
    g = rep(0.2,8),
    model = rep("3PL",8)
  )

  item_par_cols <- list(
    "3PL" = c("a","b","g")
  )

  res <- inv_tcc_from_icc(
    items,
    item_par_cols,
    "model"
  )

  theta <- res$est.theta

  icc <- compute_icc(
    items = items,
    item_par_cols = item_par_cols,
    theta = theta,
    model_col = "model"
  )

  score_vec <- 0:(ncol(icc[[1]])-1)

  tcc <- vapply(
    icc,
    function(m) sum(m %*% score_vec),
    numeric(1)
  )

  expect_true(mean(abs(tcc - res$sum.score)) < 0.5)

})
