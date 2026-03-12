testthat::skip_if_not_installed("irtQ")
test_that("analytic_mst_classification runs and returns valid structure", {

  eval_tb <- data.frame(
    theta = seq(-3, 3, 1),
    csem  = rep(0.5, 7)
  )

  theta_weight <- data.frame(
    theta = eval_tb$theta,
    w = rep(1, 7)
  )

  res <- analytic_mst_classification(
    decision_theta_cuts = c(-1, 1),
    eval_tb = eval_tb,
    theta_weight = theta_weight
  )

  expect_type(res, "list")
  expect_true(all(c("confusion", "marginal", "conditional") %in% names(res)))
  expect_equal(nrow(res$confusion), 3)
})


test_that("conditional probabilities sum to 1", {

  eval_tb <- data.frame(
    theta = seq(-2, 2, 1),
    csem  = rep(0.4, 5)
  )

  theta_weight <- data.frame(
    theta = eval_tb$theta,
    w = rep(1, 5)
  )

  res <- analytic_mst_classification(
    decision_theta_cuts = c(0),
    eval_tb = eval_tb,
    theta_weight = theta_weight
  )

  psums <- rowSums(res$confusion)
  expect_true(all(abs(sum(psums) - 1) < 1e-6))
})

test_that("analytic_mst_classification matches cac_rud", {

  skip_if_not_installed("irtQ")  # or the package providing cac_rud()

  ## ---- input setup ----
  set.seed(123)

  decision_theta_cuts <- c(-0.5, 0.5)     # corresponds to cutscore
  theta <- seq(-3, 3, length.out = 41)
  se <- abs(rnorm(41,1,0.1))

  eval_tb <- as.data.frame(cbind(theta = theta, csem = se))
  theta_weight <- gen_weight(theta = theta,dist = "norm",
                             params = list(mean = 0,sd =1))

  ## ---- reference (Rudner) ----
  ref <- irtQ::cac_rud(
    cutscore = decision_theta_cuts,
    theta = NULL,
    se = se,
    weights = theta_weight
  )

  ## ---- new implementation ----
  out <- analytic_mst_classification(
    decision_theta_cuts = decision_theta_cuts,
    eval_tb = eval_tb,
    theta_weight = theta_weight
  )

  ## ---- structure ----
  expect_type(out, "list")
  expect_named(
    out,
    c("cutscore","confusion", "marginal","conditional", "prob.level")
  )

  ## ---- equality checks ----
  expect_equal(out$cutscore, ref$cutscore, tolerance = 1e-12)

  tolerance<-1e-6
  expect_true(all(abs(as.vector(out$confusion)- as.vector(ref$confusion))<=tolerance))
  expect_true(all(abs(as.vector(out$marginal$accuracy)- as.vector(ref$marginal$accuracy))<=tolerance))
  expect_true(all(abs(as.vector(out$marginal$consistency)- as.vector(ref$marginal$consistency))<=tolerance))
  expect_true(all(abs(as.vector(out$conditional$accuracy)-as.vector(ref$conditional$accuracy))<=tolerance))
  expect_true(all(abs(as.vector(out$conditional$consistency)-as.vector(ref$conditional$consistency))<=tolerance))
  expect_true(all(abs(as.vector(out$prob.level)-as.vector(as.matrix(ref$prob.level[,4:6]))<=tolerance)))
})


test_that("analytic_mst_classification matches cac_rud across cut schemes", {

  skip_if_not_installed("irtQ")

  theta <- seq(-4, 4, length.out = 81)
  se <- rep(0.4, length(theta))
  theta_weight <- gen_weight(theta = theta,dist = "norm",
                             params = list(mean = 0,sd =1))

  eval_tb <- as.data.frame(cbind(theta = theta, csem = se))

  cut_list <- list(
    c(0),
    c(-1, 1),
    c(-1.5, -0.2, 1.2)
  )

  for (cuts in cut_list) {

    ref <- irtQ::cac_rud(
      cutscore = cuts,
      theta = theta,
      se = se,
      weights = theta_weight
    )

    out <- analytic_mst_classification(
      decision_theta_cuts = cuts,
      eval_tb = eval_tb,
      theta_weight = theta_weight
    )

    tolerance<-1e-6
    expect_true(all(abs(as.vector(out$confusion)- as.vector(ref$confusion))<=tolerance))
    expect_true(all(abs(as.vector(out$marginal$accuracy)- as.vector(ref$marginal$accuracy))<=tolerance))
    expect_true(all(abs(as.vector(out$marginal$consistency)- as.vector(ref$marginal$consistency))<=tolerance))
    expect_true(all(abs(as.vector(out$conditional$accuracy)-as.vector(ref$conditional$accuracy))<=tolerance))
    expect_true(all(abs(as.vector(out$conditional$consistency)-as.vector(ref$conditional$consistency))<=tolerance))
    expect_true(all(abs(as.vector(out$prob.level)-as.vector(as.matrix(ref$prob.level[,-(1:3)]))<=tolerance)))

  }
})
