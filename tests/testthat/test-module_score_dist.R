test_that("module_score_dist returns a matrix for single theta", {

  module_items <- data.frame(
    a = c(1.2, 0.8, 1.0),
    b = c(-0.5, 0.0, 0.5),
    c = rep(0,3),
    d = rep(1,3),
    model = rep("2PL",3)
  )

  icc_list <- compute_icc(module_items,list("2PL"=c("a", "b")),
                          theta  = 0,model_col     = "model")

  out<-module_score_dist(icc_list[[1]])
  expect_true(is.matrix(out))
  expect_equal(ncol(out), 1L)
  expect_equal(colnames(out), "theta")
  expect_equal(rownames(out), as.character(0:(nrow(out) - 1L)))

  ## probabilities sum to 1
  expect_equal(sum(out[, 1]), 1, tolerance = 1e-10)
})


test_that("module_score_dist returns a matrix for multiple thetas", {

  module_items <- data.frame(
    a = c(1.2, 0.8, 1.0),
    b = c(-0.5, 0.0, 0.5),
    c = rep(0,3),
    d = rep(1,3),
    model = rep("2PL",3)
  )

  thetas <- c(-1, 0, 1)

  icc_list <- compute_icc(module_items,list("2PL"=c("a", "b")),
                          theta  = thetas,model_col     = "model")

  out<-module_score_dist(icc_list)

  expect_true(is.matrix(out))
  expect_equal(ncol(out), length(thetas))
  expect_equal(colnames(out), paste0("theta=", thetas))
  expect_equal(rownames(out), as.character(0:(nrow(out) - 1L)))

  ## each column sums to 1
  expect_true(all(
    abs(colSums(out) - 1) < 1e-10
  ))
})


test_that("module_score_dist works for polytomous IRT models", {

  ## example: 3-category items (scores 0,1,2)
  ## parameterization delegated to compute_icc()
  module_items <- data.frame(
    a  = c(1.0, 1.2),
    b1 = c(-1.0, -0.5),
    b2 = c( 0.5,  1.0),
    model = rep("GRM",2)
  )

  icc_list <- compute_icc(module_items,list("GRM"=c("a", "b1", "b2")),
                          theta  = c(-1, 0, 1),model_col         = "model")
  out<-module_score_dist(icc_list)
  ## structure
  expect_true(is.matrix(out))
  expect_equal(ncol(out), 3L)
  expect_equal(colnames(out), c("theta=-1", "theta=0", "theta=1"))

  ## row names = possible total scores
  ## 2 items × max score 2 = 0..4
  expect_equal(
    rownames(out),
    as.character(0:4)
  )

  ## probabilities valid
  expect_true(all(out >= 0))
  expect_true(all(abs(colSums(out) - 1) < 1e-10))
})

test_that("module_score_dist supports mixed dichotomous and polytomous items", {

  module_items <- data.frame(
    model = c("2PL", "GRM"),
    a  = c(1.2, 1.0),
    b  = c(0.0, NA),
    b1 = c(NA, -1),
    b2 = c(NA,  1)
  )

  item_par_cols <- list(
    "2PL"=c("a", "b"),        # dichotomous
    "GRM"=c("a", "b1", "b2")  # polytomous
  )

  icc_list <- compute_icc(module_items,item_par_cols,
                          theta  = 0, model_col = "model")
  out<-module_score_dist(icc_list)
  expect_true(is.matrix(out))
  expect_equal(colnames(out), "theta=0")

  ## max score = 1 + 3 = 4
  expect_equal(rownames(out), as.character(0:4))
  expect_equal(sum(out), 1, tolerance = 1e-10)
})

