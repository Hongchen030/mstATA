test_that(".invert_tcc_score finds correct root for linear TCC", {

  theta_grid <- seq(-5, 5, 0.01)

  # simple linear TCC
  tcc <- theta_grid + 10

  score <- 10
  idx <- findInterval(score, tcc)

  theta_hat <- .invert_tcc_score(
    score = score,
    theta_grid = theta_grid,
    tcc = tcc,
    idx = idx,
    tol = 1e-6
  )

  expect_equal(theta_hat, 0, tolerance = 1e-5)

})


test_that(".invert_tcc_score matches analytical inverse", {

  theta_grid <- seq(-6, 6, 0.01)

  tcc <- 2 * theta_grid + 5

  score <- 9
  idx <- findInterval(score, tcc)

  theta_hat <- .invert_tcc_score(
    score,
    theta_grid,
    tcc,
    idx,
    tol = 1e-6
  )

  true_theta <- (score - 5) / 2

  expect_equal(theta_hat, true_theta, tolerance = 1e-5)

})


test_that(".invert_tcc_score agrees with uniroot solution", {

  theta_grid <- seq(-6, 6, 0.01)

  tcc <- theta_grid^3 + 10

  score <- 10.5
  idx <- findInterval(score, tcc)

  theta_fast <- .invert_tcc_score(
    score,
    theta_grid,
    tcc,
    idx,
    tol = 1e-6
  )

  f <- function(x) {
    approx(theta_grid, tcc, xout = x)$y - score
  }

  theta_true <- uniroot(f, range(theta_grid))$root

  expect_equal(theta_fast, theta_true, tolerance = 1e-4)

})


test_that(".invert_tcc_score respects tolerance", {

  theta_grid <- seq(-6, 6, 0.01)

  tcc <- theta_grid + 8

  score <- 9
  idx <- findInterval(score, tcc)

  theta_hat <- .invert_tcc_score(
    score,
    theta_grid,
    tcc,
    idx,
    tol = 1e-3
  )

  f <- function(x) approx(theta_grid, tcc, xout = x)$y - score
  theta_true <- uniroot(f, range(theta_grid))$root

  expect_equal(theta_hat, theta_true, tolerance = 1e-3)

})


test_that(".invert_tcc_score works for multiple bracket positions", {

  theta_grid <- seq(-6, 6, 0.01)
  value<- theta_grid^2 + 10
  tcc <-value[order(value,decreasing = FALSE)]

  scores <- c(10.5, 11, 12)

  for (s in scores) {

    idx <- findInterval(s, tcc)

    theta_hat <- .invert_tcc_score(
      s,
      theta_grid,
      tcc,
      idx,
      tol = 1e-4
    )

    expect_true(is.finite(theta_hat))
  }

})
