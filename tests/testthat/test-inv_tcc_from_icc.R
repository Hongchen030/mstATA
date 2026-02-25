test_that("inv_tcc_from_icc errors on invalid inputs", {

  expect_error(
    inv_tcc_from_icc(NULL, 3),
    "non-empty list"
  )

  expect_error(
    inv_tcc_from_icc(list(a = 1), NA),
    "single non-missing numeric"
  )

  expect_error(
    inv_tcc_from_icc(list(a = 1), 3),
    "must be of the form"
  )
})

make_icc_list <- function(theta_grid,
                          a = 1,
                          b = 0,
                          g = 0.2,
                          n_items = 10) {
  items<-data.frame(a=rep(a,n_items),b=rep(b,n_items),g=rep(g,n_items),
                    model = rep("3PL",n_items))
  icc_list <- compute_icc(items = items,item_par_cols = list("3PL"=c("a","b","g")),
                          theta = theta_grid,model_col = "model")
  return(icc_list)
}

test_that("inv_tcc_from_icc correctly inverts TCC for interior scores", {

  theta_grid <- seq(-5, 5, length.out = 101)
  icc_list <- make_icc_list(theta_grid)

  # choose a theta away from boundaries
  theta_true <- 0.8


  score_true <- expected_score(icc_list,target_theta = theta_true)

  theta_hat <- inv_tcc_from_icc(
    icc_list = icc_list,
    target_score = score_true,
    range_tcc = c(-5, 5)
  )

  expect_true(is.finite(theta_hat))
  expect_equal(theta_hat, theta_true, tolerance = 0.15)
})

test_that("inv_tcc_from_icc interpolates below guessing floor correctly", {

  theta_grid <- seq(-5, 5, length.out = 101)
  g <- 0.23
  icc_list <- make_icc_list(theta_grid, g = g)

  # guessing floor
  exp_scores <- vapply(icc_list, expected_score, numeric(1))
  G <- min(exp_scores)
  X <- ceiling(G)

  # theta_X from inverse TCC
  theta_X <- inv_tcc_from_icc(
    icc_list = icc_list,
    target_score = X,
    range_tcc = c(-5, 5)
  )

  # score below guessing floor
  Y <- 0

  theta_hat <- inv_tcc_from_icc(
    icc_list = icc_list,
    target_score = Y,
    range_tcc = c(-5, 5)
  )

  theta_expected <- -5 + (Y / X) * (theta_X + 5)

  expect_equal(theta_hat, theta_expected, tolerance = 1e-8)
})

test_that("inv_tcc_from_icc saturates at upper bound for large scores", {

  theta_grid <- seq(-5, 5, length.out = 101)
  icc_list <- make_icc_list(theta_grid, g = 0.2)

  theta_hat <- inv_tcc_from_icc(
    icc_list = icc_list,
    target_score = 999,
    range_tcc = c(-5, 5)
  )

  expect_equal(theta_hat, 5)
})

test_that("inv_tcc_from_icc is monotone increasing in score", {

  theta_grid <- seq(-5, 5, length.out = 101)
  icc_list <- make_icc_list(theta_grid, g = 0.2)

  scores <- 0:15
  thetas <- vapply(
    scores,
    function(s) inv_tcc_from_icc(
      icc_list = icc_list,
      target_score = s,
      range_tcc = c(-5, 5)
    ),
    numeric(1)
  )

  expect_true(all(diff(thetas) >= -1e-8))
})







