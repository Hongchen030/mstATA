test_that("Ii_internal matches closed-form Fisher information for 2PL", {

  theta <- 0.4
  D <- 1.7

  # a, b
  it <- matrix(c(1.2, -0.3,0,1), nrow = 1)

  out <- Ii_internal(theta = theta,
                     itempar_mat = it,
                     model = NULL,
                     D = D)

  # closed form
  z <- D * 1.2 * (theta + 0.3)
  P <- plogis(z)
  I_ref <- D^2 * 1.2^2 * P * (1 - P)

  expect_equal(out, I_ref, tolerance = 1e-10)
})

test_that("Ii_internal matches mstR-based Fisher information", {

  skip_if_not_installed("mstR")

  theta <- -0.2
  D <- 1.7

  models <- list(
    "NULL" = list(
      model = NULL,
      it = matrix(c(1.1, 0.2, 0.15, 1), nrow = 1)
    ),
    "GRM" = list(
      model = "GRM",
      it = matrix(c(1.0, -1, 0, 1), nrow = 1)
    ),
    "PCM" = list(
      model = "PCM",
      it = matrix(c(-1, 0, 1), nrow = 1)
    ),
    "GPCM" = list(
      model = "GPCM",
      it = matrix(c(1.2, -1, 0, 1), nrow = 1)
    ),
    "RSM" = list(
      model = "RSM",
      it = matrix(c(0.3, -1, 0, 1), nrow = 1)
    ),
    "NRM" = list(
      model = "NRM",
      it = matrix(c(1.1, -0.5, 0.8, 0.2), nrow = 1)
    )
  )

  for (nm in names(models)) {

    m <- models[[nm]]$model
    it <- models[[nm]]$it

    I_ref <- mstR::Ii(th = theta,it = it,model = m,D = D)$Ii

    I_new <- Ii_internal(theta = theta,
                         itempar_mat = it,
                         model = m,
                         D = D)

    expect_equal(I_new, I_ref, tolerance = 1e-10, label = nm)
  }
})

test_that("Ii_internal matches numeric Fisher information (GRM)", {

  skip_if_not_installed("numDeriv")

  theta <- 0.1
  D <- 1.7
  it <- matrix(c(1.0, -1, 0, 1), nrow = 1)

  # Numeric derivative of the probability vector
  P_fun <- function(th) {
    as.numeric(Pi_internal(theta = th, itempar_mat = it, model = "GRM", D = D)$Pi)
  }

  P0 <- P_fun(theta)
  dP_num <- as.numeric(numDeriv::jacobian(P_fun, theta))  # length K vector

  eps <- 1e-12
  P0 <- pmax(P0, eps)

  I_num <- sum(dP_num^2 / P0)
  I_new <- Ii_internal(theta = theta, itempar_mat = it, model = "GRM", D = D)

  expect_equal(I_new, as.numeric(I_num), tolerance = 1e-6)
})

