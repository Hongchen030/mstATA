test_that("gen_weight returns valid normalized weights for normal distribution", {
  w <- gen_weight(dist = "norm", params = list(mean = 0, sd = 1))

  expect_s3_class(w, "data.frame")
  expect_true(all(c("theta", "w") %in% names(w)))
  expect_length(w$theta, 101)
  expect_equal(sum(w$w), 1, tolerance = 1e-10)
  expect_true(all(w$w >= 0))
})

test_that("gen_weight handles empirical distribution", {
  set.seed(123)
  emp <- rnorm(500)

  w <- gen_weight(dist = "empirical", empirical_theta = emp)

  expect_equal(sum(w$w), 1, tolerance = 1e-10)
  expect_true(all(w$w >= 0))
})

test_that("empirical distribution errors without empirical_theta", {
  expect_error(
    gen_weight(dist = "empirical"),
    "Provide sample vector"
  )
})

test_that("gen_weight handles mixture distributions", {
  components <- list(
    list(dist = "norm", params = list(mean = -1, sd = 1), weight = 0.4),
    list(dist = "norm", params = list(mean = 1,  sd = 1), weight = 0.6)
  )

  w <- gen_weight(dist = "mixture", components = components)

  expect_equal(sum(w$w), 1, tolerance = 1e-10)
  expect_true(all(w$w >= 0))
})

test_that("mixture distribution errors with missing weights", {
  components <- list(
    list(dist = "norm", params = list(mean = 0, sd = 1))
  )

  expect_error(
    gen_weight(dist = "mixture", components = components),
    "Each mixture component must define 'dist' and 'weight'."
  )
})

test_that("unknown distribution errors cleanly", {
  expect_error(
    gen_weight(dist = "doesnotexist"),
    "should be one of"
  )
})


test_that("invalid parameters propagate error", {
  expect_error(
    suppressWarnings(
      gen_weight(dist = "beta", params = list(shape1 = -1, shape2 = 2))
    ),
    "Density evaluation failed"
  )
})

test_that("density with negligible mass errors", {
  expect_error(
    gen_weight(dist = "norm", params = list(mean = 10, sd = 0.01)),
    "integrates to ~0"
  )
})

test_that("gen_weight matches irtQ::gen.weight for normal distribution", {

  skip_if_not_installed("irtQ")

  params <- list(mean = 0, sd = 1)

  ref <- irtQ::gen.weight(
    dist = "norm",
    mu = 0, sigma = 1,
    theta = seq(-5,5,0.1)
  )

  out <- gen_weight(
    theta = seq(-5,5,0.1),
    dist = "norm",
    params = params
  )
  tolerance<-1e-5
  expect_true(all(abs(out$theta-ref$theta)<= tolerance))
  expect_true(all(abs(out$w-ref$weight)<= tolerance))
})
