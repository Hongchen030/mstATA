testthat::skip_if_not_installed("mstR")

test_that("Pi_internal matches mstR::Pi for dichotomous models", {

  theta <- 0.4
  D <- 1.7

  it <- matrix(c(1.2, -0.3, 0.2, 1), nrow = 1)

  ref <- mstR::Pi(th = theta, it = it, model = NULL, D = D)
  new <- Pi_internal(theta = theta, itempar_mat = it, model = NULL, D = D)

  expect_equal(new$Pi,   ref$Pi,   tolerance = 1e-10)
  expect_equal(new$dPi,  ref$dPi,  tolerance = 1e-10)
})


test_that("Pi_internal matches mstR::Pi for GRM", {

  skip_if_not_installed("mstR")

  theta <- -0.5
  D <- 1.7

  it <- matrix(c(1.1, -1.2, 0.0, 1.3), nrow = 1)

  ref <- mstR::Pi(th = theta, it = it, model = "GRM", D = D)
  new <- Pi_internal(theta = theta, itempar_mat = it, model = "GRM", D = D)

  expect_equal(as.vector(new$Pi), as.vector(ref$Pi),   tolerance = 1e-10)
  expect_equal(as.vector(new$dPi),  as.vector(ref$dPi),  tolerance = 1e-10)
})


test_that("Pi_internal matches mstR::Pi for polytomous models", {

  skip_if_not_installed("mstR")

  theta <- 0.8
  D <- 1.7

  models <- list(
    PCM  = matrix(c(-1, 0, 1), nrow = 1),
    GPCM = matrix(c(1.2, -1, 0, 1), nrow = 1),
    RSM  = matrix(c(0.3, -1, 0, 1), nrow = 1),
    NRM  = matrix(c(1.1, -0.5, 0.8, 0.2), nrow = 1)
  )

  for (m in names(models)) {
    it <- models[[m]]

    ref <- mstR::Pi(th = theta, it = it, model = m, D = D)
    new <- Pi_internal(theta = theta, itempar_mat = it, model = m, D = D)

    expect_equal(as.vector(new$Pi), as.vector(ref$Pi),   tolerance = 1e-10,label = m)
    expect_equal(as.vector(new$dPi),  as.vector(ref$dPi),  tolerance = 1e-10,label = m)
  }
})
